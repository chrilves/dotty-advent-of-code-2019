import scala.math._
import scala.annotation._

object Day5 extends Day {
  type Input = List[Int]
  type Output = Int

  def parseInput(s: String): Input = 
    s.split(",").map(_.toInt).toList

  trait StateIO[A] {
    def (a:A) in: (Int, A)
    def (a:A) out(i: Int): A
  }

  def partOne(i: Input): Output = {
    val program: Array[Int] = i.toArray

    @tailrec
    def run[S : StateIO](stateIO: S, addr : Int = 0): S = {
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
          val (in, state2) = stateIO.in
          program(program(addr + 1)) = in
          run(state2, addr + 2)
        
        case 4 => // Write
          val state2 = stateIO.out(arg(1))
          run(state2, addr + 2)
        
        case 99 => // Halt
          stateIO
      }
    }

    type State = (Boolean, Option[Int])

    given StateIO[State] {
      def (a:State) in: (Int, State) =
        if a._1
        then (1, (false, a._2))
        else throw new Exception("Second Input Command")
  
      def (a:State) out(i: Int): State = {
        println(i)
        (a._1, Some(i))
      }
    }

    run[State]((true, None))._2 match {
      case Some(diagnosticCode) => diagnosticCode
      case _                    => throw new Exception("No output")
    }
  }


  def partTwo(i: Input): Output = {
    val program: Array[Int] = i.toArray

    @tailrec
    def run[S : StateIO](stateIO: S, addr : Int = 0): S = {
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
          val (in, state2) = stateIO.in
          program(program(addr + 1)) = in
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

    type State = (Boolean, Option[Int])

    given StateIO[State] {
      def (a:State) in: (Int, State) =
        if a._1
        then (5, (false, a._2))
        else throw new Exception("Second Input Command")
  
      def (a:State) out(i: Int): State = {
        println(i)
        (a._1, Some(i))
      }
    }

    run[State]((true, None))._2 match {
      case Some(diagnosticCode) => diagnosticCode
      case _                    => throw new Exception("No output")
    }
  }
}