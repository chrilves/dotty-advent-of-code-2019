import scala.annotation._

object IntCode {

  trait StateIO[+I,-O,A] {
    def (a:A) read : (I, A)
    def (a:A) write(x: O): A
  }

  type PreProcess[-I,+BI,+O,+A] = Either[A, I => Process[I,BI,O,A]]

  sealed abstract class Process[-I,+BI,+O,+A] {
    import Process._

    def output: Vector[O]

    inline private def addOuts[O2>:O](outs: Vector[O2]): Process[I,BI,O2,A] =
      this match {
        case End(ins, result, outs2) => End(ins, result, outs2 ++ outs)
        case Read(k, outs2) => Read(k, outs2 ++ outs)
      }

    final def collectOutput: (Process[I,BI,O,A], Vector[O]) =
      this match {
        case End(ins, a, outs) => (End(ins, a, Vector.empty), outs)
        case Read(cont, outs) => (Read(cont, Vector.empty), outs)
      }

    final def send[I2<:I](i: I2): Process[I,I2|BI,O,A] =
      this match {
        case End(ins, result, outs) => End(i +: ins, result, outs)
        case Read(cont, outs) => cont(i).addOuts(outs)
      }

    @tailrec
    final def sends[I2<:I](ins: Vector[I2]): Process[I,I2|BI,O,A] =
      if ins.isEmpty
      then this
      else this match {
        case End(ins2, r, outs) => End(ins ++ ins2, r, outs)
        case Read(cont, outs) => cont(ins.last) match {
          case End(ins2, result, outs2) => End(ins.init ++ ins2, result, outs2 ++ outs)
          case Read(k, outs2) => Read(k, outs2 ++ outs).sends(ins.init)
        }
      }

    final def map[B](f: A => B): Process[I,BI,O,B] =
      this match {
        case End(ins, a, outs) => End(ins, f(a), outs)
        case Read(cont, outs) => Read((i:I) => cont(i).map(f), outs)
      }
    
    final def flatMap[I2>:BI,BI2>:BI,O2>:O,B](f: A => Process[I2,BI2,O2,B]): Process[I&I2,BI2,O2,B] =
      this match {
        case End(ins, a, outs) => f(a).sends(ins).addOuts(outs)
        case Read(cont, outs)  => Read((i:I) => cont(i).flatMap(f), outs)
      }

    final def toPreProcess: End[BI,O, PreProcess[I,BI,O,A]] =
      this match {
        case End(ins, a, outs) => End(ins, Left(a), outs)
        case Read(cont, outs)  => End(Vector.empty, Right(cont), outs) 
      }

    final def chain[O2>:O, BO, X, B](p: Process[O2, BO, X, B]): Process[I, BI, X, (PreProcess[I,BI,O,A], Process[O2,BO|O2,X,B])] = {
      val (p1, outs1) = this.collectOutput
      val (p2, outs2) = p.sends(outs1).collectOutput

      (p1,p2) match {
        case (Read(cont1, _) , Read(_,_)    ) => Read((i:I) => cont1(i).chain(p2), outs2)
        case (Read(cont1, _) , End(insb,b,_)) => End(Vector.empty, (Right(cont1), p2), outs2)
        case (End(insa, a, _), _            ) => End(insa        , (Left(a)     , p2), outs2)
      }
    }

    @tailrec
    final def duo[I2<:I, O2>:O, BO,B](p: Process[O2, BO, I2, B]): (Process[I,I2|BI,O,A], Process[O2,O|BO,I2,B]) =
      if (this.output.isEmpty && p.output.isEmpty)
      then (this, p)
      else {
        val (p1, outs1) = this.collectOutput
        val (p2, outs2) = p.collectOutput
        p1.sends(outs2).duo(p2.sends(outs1))
      }  

    @tailrec
    final def loop[I2<:I,O2>:O<:I2]: Process[I,O2|BI,O,A] =
      if this.output.isEmpty
      then this
      else {
        val (p, outs) = this.collectOutput
        (p.sends(outs: Vector[O2]):Process[I, O2|BI, O, A]).loop[I2,O2]
      }
/*
    @tailrec
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
  */
  }

  object Process {
    final case class End[+BI,+O,+A](input: Vector[BI], result: A, output: Vector[O]) extends Process[Any,BI,O,A]
    final case class Read[-I,+BI,+O,+A](cont: I => Process[I,BI,O,A], output: Vector[O]) extends Process[I,BI,O,A]

    inline def pure[A](a:A): Process[Any,Nothing,Nothing,A] =
      End[Nothing,Nothing,A](Vector.empty, a, Vector.empty)
    
    inline def input[I]: Process[I,Nothing,Nothing,I] =
      Read((i:I) => End(Vector.empty, i, Vector.empty), Vector.empty)
    
    inline def output[O](o : O*): Process[Any, Nothing, O, Unit] =
      End(Vector.empty, (), o.toVector)


    inline private def addOuts[I,BI,O,A](self: Process[I,BI,O,A], outs: Vector[O]): Process[I,BI,O,A] =
      self match {
        case End(ins, result, outs2) => End(ins, result, outs2 ++ outs)
        case Read(k, outs2) => Read(k, outs2 ++ outs)
      }
    
    final def send[I,BI>:I,O,A](self: Process[I,BI,O,A], i: I): Process[I,I|BI,O,A] =
      self match {
        case End(ins, result, outs) => End(i +: ins, result, outs)
        case Read(cont, outs) => addOuts(cont(i), outs)
      }
  
    @tailrec
    final def sends[I,BI,O,A](self: Process[I,BI,O,A], ins: Vector[I]): Process[I,I|BI,O,A] =
      if ins.isEmpty
      then self
      else self match {
        case End(ins2, r, outs) => End(ins ++ ins2, r, outs)
        case Read(cont, outs) => cont(ins.last) match {
          case End(ins2, result, outs2) => End(ins.init ++ ins2, result, outs2 ++ outs)
          case Read(k, outs2) => sends(Read(k, outs2 ++ outs), ins.init)
        }
      }
  
      
    final def flatMap[I,BI>:I,O,A,B](self: Process[I,BI,O,A], f: A => Process[BI,BI,O,B]): Process[I,BI,O,B] =
      self match {
        case End(ins, a, outs) => addOuts(sends(f(a), ins), outs)
        case Read(cont, outs)  => Read((i:I) => flatMap(cont(i), f), outs)
      }
    
    final def chain[I,BI,O,BO,X,A,B](self: Process[I,BI,O,A], p: Process[O, BO, X, B]): Process[I, BI, X, (PreProcess[I,BI,O,A], Process[O,O|BO,X,B])] = {
      val (p1, outs1) = self.collectOutput
      val (p2, outs2) = p.sends(outs1).collectOutput

      (p1,p2) match {
        case (Read(cont1, _) , Read(_,_)    ) => Read((i:I) => chain(cont1(i), p2), outs2)
        case (Read(cont1, _) , End(insb,b,_)) => End(Vector.empty, (Right(cont1), p2), outs2)
        case (End(insa, a, _), _            ) => End(insa        , (Left(a)     , p2), outs2)
      }
    }
  
    @tailrec
    final def duo[I,BI,O,BO,A,B](self: Process[I,BI,O,A], p: Process[O, BO, I, B]): (Process[I,I|BI,O,A], Process[O,O|BO,I,B]) =
      if (self.output.isEmpty && p.output.isEmpty)
      then (self, p)
      else {
        val (p1, outs1) = self.collectOutput
        val (p2, outs2) = p.collectOutput
        duo(sends(p1, outs2), sends(p2, outs1))
      }
  
    @tailrec
    final def loop[I,BI,O<:I,A](self: Process[I,BI,O,A]): Process[O,O|BI,O,A] =
      if self.output.isEmpty
      then self
      else {
        val (p, outs) = self.collectOutput
        loop(sends(p, outs))
      }
    
    def ofPreProcess[I,BI,O,A](pp: PreProcess[I,BI,O,A]): Process[I,BI,O,A] =
      pp match {
        case Left(a)     => Process.pure(a)
        case Right(cont) => Read(cont, Vector.empty)
      }

    def unsafeFromProgram(prg: Array[BigInt]): Process[BigInt, Nothing, BigInt, Unit] = {
      import Process._
  
      val program = Array.tabulate(100 * prg.size) { pos =>
        if pos < prg.size
        then prg(pos)
        else BigInt(0)
      }

      def runTrick(base: Int, addr : Int, out: List[BigInt]): Process[BigInt, Nothing, BigInt, Unit] =
        run(base, addr, out)

      @tailrec
      def run(base: Int, addr : Int, out: List[BigInt]): Process[BigInt, Nothing, BigInt, Unit] = {
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
            run(base, addr + 4, out)
  
          case 2 => // Multiplier
            program(pointer(3)) = arg(1) * arg(2)
            run(base, addr + 4, out)
  
          case 3 => // Read
            Read( { cin =>
              program(pointer(1)) = cin
              runTrick(base, addr + 2, Nil)
            }, out.reverse.toVector)
  
          case 4 => // Write
            val cout = arg(1)
            run(base, addr + 2, cout :: out)
  
          case 5 => // Jump if true
            if arg(1) != 0
            then run(base, arg(2).toInt, out)
            else run(base, addr + 3    , out)
  
          case 6 => // Jump if false
            if arg(1) == 0
            then run(base, arg(2).toInt, out)
            else run(base, addr + 3    , out)
  
          case 7 => // Less than
            program(pointer(3)) = if arg(1) < arg(2) then 1 else 0
            run(base, addr + 4, out)
  
          case 8 => // Equal
            program(pointer(3)) = if arg(1) == arg(2) then 1 else 0
            run(base, addr + 4, out)
  
          case 9 => // Change base
            val newBase = base + arg(1).toInt
            run(newBase, addr + 2, out)
  
          case 99 => // Halt
            End(Vector.empty, (), out.reverse.toVector)
        }
      }
  
      run(0,0, Nil)
    }
    
    def fromProgram(prg: Array[BigInt]): Process[BigInt, Nothing, BigInt, Unit] = {
      import Process._
  
      val initialProgram: Map[Int, BigInt] = {
        val mb = Map.newBuilder[Int, BigInt]
        for
          i <- 0 to (prg.size - 1)
        do mb += i -> prg(i)

        mb.result
      }

      def runTrick(prog: Map[Int, BigInt], base: Int, addr : Int, out: List[BigInt]): Process[BigInt, Nothing, BigInt, Unit] =
        run(prog, base, addr, out)

      @tailrec
      def run(prog: Map[Int, BigInt], base: Int, addr : Int, out: List[BigInt]): Process[BigInt, Nothing, BigInt, Unit] = {
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
            run(prog2, base, addr + 4, out)
  
          case 2 => // Multiplier
            val prog2 = prog + (pointer(3) -> (arg(1) * arg(2)))
            run(prog2, base, addr + 4, out)
  
          case 3 => // Read
            Read( { cin =>
              val prog2 = prog + (pointer(1) -> cin)
              runTrick(prog2, base, addr + 2, Nil)
            }, out.reverse.toVector)
  
          case 4 => // Write
            val cout = arg(1)
            run(prog, base, addr + 2, cout :: out)
  
          case 5 => // Jump if true
            if arg(1) != 0
            then run(prog, base, arg(2).toInt, out)
            else run(prog, base, addr + 3    , out)
  
          case 6 => // Jump if false
            if arg(1) == 0
            then run(prog, base, arg(2).toInt, out)
            else run(prog, base, addr + 3    , out)
  
          case 7 => // Less than
            val prog2 = prog + (pointer(3) -> BigInt(if arg(1) < arg(2) then 1 else 0))
            run(prog2, base, addr + 4, out)
  
          case 8 => // Equal
            val prog2 = prog + (pointer(3) -> BigInt(if arg(1) == arg(2) then 1 else 0))
            run(prog2, base, addr + 4, out)
  
          case 9 => // Change base
            val newBase = base + arg(1).toInt
            run(prog, newBase, addr + 2, out)
  
          case 99 => // Halt
            End(Vector.empty, (), out.reverse.toVector)
        }
      }
      run(initialProgram, 0,0, Nil)
    }
  }
}