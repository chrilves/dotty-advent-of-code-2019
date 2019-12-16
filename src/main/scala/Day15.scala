import scala.math.{log => _, _}
import scala.annotation._

object Day15 extends Day {
  type Input = Array[BigInt]
  type Output = String

  def parseInput(s: String): Input =
    s.split(",").map(BigInt(_))

  final case class Pos(x: Int, y: Int) {
    override def toString = s"($x,$y)"
  }

  enum Or[+A,+B] {
    case Left(a: A)
    case Right(b: B)
    case Both(a: A, b: B)
  }

  trait StateIO[+I,-O,A] {
    def (a:A) read : (I, A)
    def (a:A) write(x: O): A
  }

  enum Process[-I,+O,+A] {
    case End(end: A)
    case Input(cont: I => Process[I,O,A])
    case Output(output: O, tail: Process[I,O,A])

    final def triMap[I2, O2, A2](fi: I2 => I, fo: O => O2, fa: A => A2): Process[I2,O2,A2] = {
      def aux(p: Process[I,O,A]): Process[I2,O2,A2] =
        p match {
          case End(a) => End(fa(a))
          case Input(k) => Input((i2:I2) => aux(k(fi(i2))))
          case Output(o, t) => Output(fo(o), aux(t))
        }

      aux(this)
    }

    final def map[B](f: A => B): Process[I,O,B] =
      this match {
        case End(a) => End(f(a))
        case Input(cont) => Input((i:I) => cont(i).map(f))
        case Output(out, tail) => Output(out, tail.map(f))
      }

    final def flatMap[I2 <: I, O2 >: O, B](f: A => Process[I2,O2,B]): Process[I2,O2,B] =
      this match {
        case End(a) => f(a)
        case Input(cont) => Input((i:I) => cont(i).flatMap(f))
        case Output(out, tail) => Output(out, tail.flatMap(f))
      }
    
    final def chain[X, B, C](p: Process[O, X, B]): Process[I, X, Or[A,B]] =
      (this, p) match {
        case (_            , Output(o, t)) => Output(o, this.chain(t))
        case (Input(cont1) , _           ) => Input((i:I) => cont1(i).chain(p))
        case (Output(o, t1), Input(cont2)) => t1.chain(cont2(o))
        case (Output(_, _) , End(b))       => End(Or.Right(b))
        case (End(a)       , End(b))       => End(Or.Both(a,b))
        case (End(a)       , Input(_))     => End(Or.Left(a))
      }

    final def duo[X, B, C](p: Process[O, I, B]): (Option[A], Option[B]) = {
      def aux(ina: Vector[I], pa: Process[I,O,A], inb: Vector[O], pb: Process[O,I,B]): (Option[A], Option[B]) =
        (pa, pb) match {
          case (Output(o, t), _           ) => aux(ina, t, inb :+ o, pb)
          case (_           , Output(i, t)) => aux(ina :+ i, pa, inb, t)
          case (Input(f)    , _           ) if ina.nonEmpty =>
            aux(ina.tail, f(ina.head), inb, pb)
          case (_           , Input(f)    ) if inb.nonEmpty =>
            aux(ina, pa, inb.tail, f(inb.head))
          case (_           , _           ) =>
            ( pa match {
                case End(a) => Some(a)
                case _ => None
              } 
            , pb match {
                case End(b) => Some(b)
                case _ => None
              }
            )
        }
      
      aux(Vector.empty, this, Vector.empty, p)
    }

    final def run[S: [X] =>> StateIO[I,O,X]](s: S): (A, S) =
      this match {
        case End(a) =>
          (a, s)
        case Input(cont) =>
          val (r, s2) = s.read
          cont(r).run(s2)
        case Output(o, t) =>
          t.run(s.write(o))
      }
  }

  object Process {
    def pure[A](a:A): Process[Any,Nothing,A] = Process.End(a)

    def input[I]: Process[I,Nothing, I] =
      Process.Input(Process.End(_))
    
    def output[O](o: O): Process[Any, O, Unit] =
      Process.Output(o, Process.End(()))

    def [X,A](p: Process[X,X,A]) loop: Process[X,X,A] = {
      def aux(outs: Vector[X], prg: Process[X,X,A]): Process[X,X,A] =
        prg match {
          case End(_) => outs.foldRight(prg) { case (x, p2) => Output(x,p2) }
          case Output(o, p2) => aux(outs :+ o, p2)
          case Input(f) =>
            if outs.isEmpty
            then Input((x:X) => aux(Vector.empty, f(x)))
            else aux(outs.tail, f(outs.head))
        }
      aux(Vector.empty, p)
    } 

    def unsafeFromProgram(prg: Array[BigInt]): Process[BigInt, BigInt, Unit] = {
      import Process._
  
      val program = Array.tabulate(100 * prg.size) { pos =>
        if pos < prg.size
        then prg(pos)
        else BigInt(0)
      }
  
      def run(base: Int, addr : Int): Process[BigInt, BigInt, Unit] = {
        val opcodeAndModes : BigInt = program(addr)
  
        inline def digit(n: Int): Int =
          if n <= 1
          then 100
          else 10 * digit(n - 1)
  
        inline def pointer(pos: Int): Int =
          (opcodeAndModes / digit(pos) % 10) match {
            case 0 => program(addr + pos).toInt
            case 1 => addr + pos
            case 2 => base + program(addr + pos).toInt
          }
  
        inline def arg(pos: Int): BigInt = program(pointer(pos))
  
        opcodeAndModes % 100 match {
          case 1 => // Adder
            program(pointer(3)) = arg(1) + arg(2)
            run(base, addr + 4)
  
          case 2 => // Multiplier
            program(pointer(3)) = arg(1) * arg(2)
            run(base, addr + 4)
  
          case 3 => // Read
            Input { cin =>
              program(pointer(1)) = cin
              run(base, addr + 2)
            }
  
          case 4 => // Write
            val cout = arg(1)
            Output(cout, run(base, addr + 2))
  
          case 5 => // Jump if true
            if arg(1) != 0
            then run(base, arg(2).toInt)
            else run(base, addr + 3)
  
          case 6 => // Jump if false
            if arg(1) == 0
            then run(base, arg(2).toInt)
            else run(base, addr + 3)
  
          case 7 => // Less than
            program(pointer(3)) = if arg(1) < arg(2) then 1 else 0
            run(base, addr + 4)
  
          case 8 => // Equal
            program(pointer(3)) = if arg(1) == arg(2) then 1 else 0
            run(base, addr + 4)
  
          case 9 => // Change base
            val newBase = base + arg(1).toInt
            run(newBase, addr + 2)
  
          case 99 => // Halt
            End(())
        }
      }
  
      run(0,0)
    }
    
    def fromProgram(prg: Array[BigInt]): Process[BigInt, BigInt, Unit] = {
      import Process._
  
      val initialProgram: Map[Int, BigInt] = {
        val mb = Map.newBuilder[Int, BigInt]
        for
          i <- 0 to (prg.size - 1)
        do mb += i -> prg(i)

        mb.result
      }

      def run(prog: Map[Int, BigInt], base: Int, addr : Int): Process[BigInt, BigInt, Unit] = {
        inline def program(i: Int): BigInt =
          prog.getOrElse(i, BigInt(0))
      
        val opcodeAndModes : BigInt = program(addr)
  
        inline def digit(n: Int): Int =
          if n <= 1
          then 100
          else 10 * digit(n - 1)
  
        inline def pointer(pos: Int): Int =
          (opcodeAndModes / digit(pos) % 10) match {
            case 0 => program(addr + pos).toInt
            case 1 => addr + pos
            case 2 => base + program(addr + pos).toInt
          }
  
        inline def arg(pos: Int): BigInt = program(pointer(pos))
  
        opcodeAndModes % 100 match {
          case 1 => // Adder
            val prog2 = prog + (pointer(3) -> (arg(1) + arg(2)))
            run(prog2, base, addr + 4)
  
          case 2 => // Multiplier
            val prog2 = prog + (pointer(3) -> (arg(1) * arg(2)))
            run(prog2, base, addr + 4)
  
          case 3 => // Read
            Input { cin =>
              val prog2 = prog + (pointer(1) -> cin)
              run(prog2, base, addr + 2)
            }
  
          case 4 => // Write
            val cout = arg(1)
            Output(cout, run(prog, base, addr + 2))
  
          case 5 => // Jump if true
            if arg(1) != 0
            then run(prog, base, arg(2).toInt)
            else run(prog, base, addr + 3)
  
          case 6 => // Jump if false
            if arg(1) == 0
            then run(prog, base, arg(2).toInt)
            else run(prog, base, addr + 3)
  
          case 7 => // Less than
            val prog2 = prog + (pointer(3) -> BigInt(if arg(1) < arg(2) then 1 else 0))
            run(prog2, base, addr + 4)
  
          case 8 => // Equal
            val prog2 = prog + (pointer(3) -> BigInt(if arg(1) == arg(2) then 1 else 0))
            run(prog2, base, addr + 4)
  
          case 9 => // Change base
            val newBase = base + arg(1).toInt
            run(prog, newBase, addr + 2)
  
          case 99 => // Halt
            End(())
        }
      }
  
      run(initialProgram, 0,0)
    }
  }


  enum Dir {
    case North, South, East, West
  }

  enum Status {
    case Wall, Ok, Oxygen
  }

  def partOne(i: Input): Output = {
    val program : Process[Dir, Status, Unit] =
      Process.fromProgram(i).triMap(
        (d: Dir) => BigInt(d match {
          case Dir.North => 1
          case Dir.South => 2
          case Dir.West  => 3
          case Dir.East  => 4   
        }),
        (bi: BigInt) => bi match {
          case 0 => Status.Wall
          case 1 => Status.Ok
          case 2 => Status.Oxygen
        },
        x => x
      )
    
    import Process._

    lazy val driver: Process[Status, Dir, Unit] =
      for
        _ <- output(Dir.North)
        status <- input[Status]
        _ = log(s"Recieved status $status")
        _ <- driver
      yield ()
    
    program.duo(driver)
    ""
  }
    

  def partTwo(i: Input): Output =
    partOne(i)
}