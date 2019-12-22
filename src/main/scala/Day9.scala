package dottyaoc

import scala.math.{log => _, _}
import scala.annotation._

object Day9 extends Day {
  type Input = Array[BigInt]
  type Output = BigInt

  def parseInput(s: String): Input = 
    s.split(",").map(BigInt(_))

  enum Program {
    case Halted
    case Input(feed: BigInt => Program)
    case Output(head: BigInt, tail: Program)
    
    final def in(x: BigInt): Program =
      this match {
        case Halted      => throw new Exception("No input")
        case Input(f)    => f(x)
        case Output(h,t) => Output(h, t.in(x))
      }
    
    final def run: BigInt =
      this match {
        case Halted   => throw new Exception("No output")
        case Input(_) => throw new Exception("No input")
        case Output(o, t) =>
          println(o)
          if t == Halted
          then o
          else t.run
      }
  }

  import Program._

  def (input: Array[BigInt]) readProgram: Program = {
    val program = Array.tabulate(100 * input.size) { pos =>
      if pos < input.size
      then input(pos)
      else BigInt(0)
    }

    def run(base: Int, addr : Int): Program = {
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
          Halted
      }
    }

    run(0,0)
  }


  def partOne(i: Input): Output =
    i.readProgram.in(1).run

  def partTwo(i: Input): Output =
    i.readProgram.in(2).run
}