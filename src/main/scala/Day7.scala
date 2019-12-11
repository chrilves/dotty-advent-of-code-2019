import scala.math.{log => _, _}
import scala.annotation._

object Day7 extends Day {
  type Input = List[Int]
  type Output = Int

  def parseInput(s: String): Input = 
    s.split(",").map(_.toInt).toList

  def partOne(i: Input): Output = {

    def runAmplifier(phase: Int, input: Int): Int = {
      val program = i.toArray

      type State = (List[Int], Option[Int])

      def (a:State) in: (Int, State) =
        a._1 match {
          case h :: t => (h, (t, a._2))
          case _      => throw new Exception("Second Input Command")
        }
  
      def (a:State) out(i: Int): State =
        (a._1, Some(i))
      
      @tailrec
      def run(stateIO: State, addr : Int = 0): State = {
        val opcodeAndModes = program(addr)
    
        inline def digit(n: Int): Int =
          if n <= 1
          then 100
          else 10 * digit(n - 1)
    
        inline def arg(pos: Int): Int =
          if (opcodeAndModes / digit(pos) % 10) == 0
          then program(program(addr + pos))
          else program(addr + pos)
        
        (opcodeAndModes % 100) match {
          case 1 => // Adder
            program(program(addr + 3)) = arg(1) + arg(2)
            run(stateIO, addr + 4)
          
          case 2 => // Multiplier
            program(program(addr + 3)) = arg(1) * arg(2)
            run(stateIO, addr + 4)
          
          case 3 => // Read
            val (cin, state2) = stateIO.in
            program(program(addr + 1)) = cin
            run(state2, addr + 2)
          
          case 4 => // Write
            val state2 = stateIO.out(arg(1))
            run(state2, addr + 2)
          
          case 5 => // Jump if true
            if arg(1) != 0
            then run(stateIO, arg(2))
            else run(stateIO, addr + 3)
          
          case 6 => // Jump if false
            if arg(1) == 0
            then run(stateIO, arg(2))
            else run(stateIO, addr + 3)
          
          case 7 => // Less than
            program(program(addr + 3)) = if arg(1) < arg(2) then 1 else 0
            run(stateIO, addr + 4)
          
          case 8 => // Equal
            program(program(addr + 3)) = if arg(1) == arg(2) then 1 else 0
            run(stateIO, addr + 4)
    
          case 99 => // Halt
            stateIO
        }
      }
  
      run((List(phase, input), None))._2 match {
        case Some(diagnosticCode) => diagnosticCode
        case _                    => throw new Exception("No output")
      }      
    }


    def (l: Set[Int]) choose: Iterator[(Int, Set[Int])] =
      l.iterator.map(x => (x, l - x))

    val answers =
      for
        (phaseA,restA) <- Set(0,1,2,3,4).choose
        (phaseB,restB) <- restA.choose
        (phaseC,restC) <- restB.choose
        (phaseD,restD) <- restC.choose
        (phaseE,_    ) <- restD.choose
      yield {
        val outA = runAmplifier(phaseA, 0)
        val outB = runAmplifier(phaseB, outA)
        val outC = runAmplifier(phaseC, outB)
        val outD = runAmplifier(phaseD, outC)
        val outE = runAmplifier(phaseE, outD)
        outE
      }

    answers.max
  }

  def partTwo(i: Input): Output = {
    enum Program {
      case Halted
      case Input(run: Int => Program)
      case Output(head: Int, tail: Program)
      
      final def in(x: Int): Program =
        this match {
          case Halted      => throw new Exception("No input")
          case Input(f)    => f(x)
          case Output(h,t) => Output(h, t.in(x))
        }

      final def chain(p: Program): Program =
        (this,p) match {
          case (_          , Output(h,t)) => Output(h, this.chain(t))
          case (Input(f)   , _          ) => Input((x:Int) => f(x).chain(p))
          case (Output(h,t), Input(f)   ) => t.chain(f(h))
          case (Halted     , Halted     ) => Halted
        }
      
      
      final def loop(lastSeen: Option[Int] = None): Option[Int] = 
        this match {
          case Halted => lastSeen
          case Output(h, Halted)   => Some(h)
          case Output(h, Input(f)) => f(h).loop(Some(h)) 
          case Output(h, p       ) => p.loop(Some(h))
          case Input(_)            => throw new Exception("No Input")
        }
    }

    import Program._

    def runAsStream(name: String): Program = {
      val program = i.toArray

      def run(addr : Int = 0): Program = {
        val opcodeAndModes = program(addr)
    
        inline def digit(n: Int): Int =
          if n <= 1
          then 100
          else 10 * digit(n - 1)
    
        inline def arg(pos: Int): Int =
          if (opcodeAndModes / digit(pos) % 10) == 0
          then program(program(addr + pos))
          else program(addr + pos)
        
        (opcodeAndModes % 100) match {
          case 1 => // Adder
            program(program(addr + 3)) = arg(1) + arg(2)
            run(addr + 4)
          
          case 2 => // Multiplier
            program(program(addr + 3)) = arg(1) * arg(2)
            run(addr + 4)
          
          case 3 => // Read
            Input { cin =>
              log(s"[$name] Reading $cin")
              program(program(addr + 1)) = cin
              run(addr + 2)
            }
          
          case 4 => // Write
            val cout = arg(1)
            log(s"[$name] Writing $cout")
            Output(cout, run(addr + 2))
          
          case 5 => // Jump if true
            if arg(1) != 0
            then run(arg(2))
            else run(addr + 3)
          
          case 6 => // Jump if false
            if arg(1) == 0
            then run(arg(2))
            else run(addr + 3)
          
          case 7 => // Less than
            program(program(addr + 3)) = if arg(1) < arg(2) then 1 else 0
            run(addr + 4)
          
          case 8 => // Equal
            program(program(addr + 3)) = if arg(1) == arg(2) then 1 else 0
            run(addr + 4)
    
          case 99 => // Halt
            Halted
        }
      }

      run()
    }

    def (l: Set[Int]) choose: Iterator[(Int, Set[Int])] =
      l.iterator.map(x => (x, l - x))

    val answers =
      for
        (phaseA,restA) <- Set(5,6,7,8,9).choose
        (phaseB,restB) <- restA.choose
        (phaseC,restC) <- restB.choose
        (phaseD,restD) <- restC.choose
        (phaseE,_    ) <- restD.choose
      yield {
        runAsStream("A").in(phaseA)
         .chain(runAsStream("B").in(phaseB))
         .chain(runAsStream("C").in(phaseC))
         .chain(runAsStream("D").in(phaseD))
         .chain(runAsStream("E").in(phaseE))
         .in(0)
         .loop(None).getOrElse(throw new Exception("No Out"))
      }

    answers.max
  }
}