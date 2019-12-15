import scala.math.{log => _, _}
import scala.annotation._

object Day11 extends Day {
  type Input = Array[BigInt]
  type Output = String

  def parseInput(s: String): Input = 
    s.split(",").map(BigInt(_))

  trait StateIO[A] {
    def (a:A) read : (BigInt, A)
    def (a:A) write(x: BigInt): A
  }


  enum Turn {
    case Left, Right
  }

  enum Direction {
    final def turn(t: Turn): Direction =
      (this, t) match {
        case (Up, Turn.Left)  => Left
        case (Up, Turn.Right) => Right
        case (Down, Turn.Left) => Right
        case (Down, Turn.Right) => Left
        case (Left, Turn.Left) => Down
        case (Left, Turn.Right) => Up
        case (Right, Turn.Left) => Up
        case (Right, Turn.Right) => Down
      }

    case Up, Down, Left, Right
  }

  enum Color {
    case White, Black
  }

  final case class Pos(x: Int, y: Int) {
    def move(dir: Direction): Pos =
      dir match {
        case Direction.Up => Pos(x, y + 1)
        case Direction.Down => Pos(x, y - 1)
        case Direction.Left => Pos(x - 1, y)
        case Direction.Right => Pos(x + 1, y)
      }
  }

  enum Command {
    case Paint, Move
  }

  final case class ViewPort(minX: Int, maxX: Int, minY: Int, maxY: Int) {
    def width = maxX - minX + 1
    def height = maxY - minY + 1
  }

  final case class State(pos: Pos, dir: Direction, hull: Map[Pos, Color], command: Command) {

    val viewPort: ViewPort  =
      hull.keysIterator.foldLeft(ViewPort(0,0,0,0)) {
        case (v, p) => ViewPort(min(p.x, v.minX), max(p.x, v.maxX), min(p.y, v.minY), max(p.y, v.maxY))
      }

    def message: String =
      Array.tabulate(viewPort.height, viewPort.width) { (y,x) =>
        hull.getOrElse(Pos(x + viewPort.minX, viewPort.maxY - y), Color.Black) match {
          case Color.Black => " "
          case Color.White => "#"
        }
      }.map(_.mkString).mkString("\n")
  }

  given stateIO : StateIO[State] {
    def (state:State) read : (BigInt, State) = {
      val color = state.hull.getOrElse(state.pos, Color.Black)
      
      val input =
        color match {
          case Color.Black => BigInt(0)
          case Color.White => BigInt(1)
        }

      input -> state
    }

    def (state:State) write(x: BigInt): State =
      state.command match {
        case Command.Paint =>
          val color =
            x match {
              case n if n == BigInt(0) => Color.Black
              case n if n == BigInt(1) => Color.White
            }
          State(state.pos, state.dir, state.hull + (state.pos -> color), Command.Move)
        case Command.Move =>
          val turn =
            x match {
              case n if n == BigInt(0) => Turn.Left
              case n if n == BigInt(1) => Turn.Right
            }
          val newDir = state.dir.turn(turn)
          State(state.pos.move(newDir),newDir, state.hull, Command.Paint)
      }
  }

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
    
    final def run[A: StateIO](a: A): A =
      this match {
        case Halted   =>
          a
        case Input(f) =>
          val (x, a2) = a.read
          f(x).run(a2)

        case Output(x, t) =>
          t.run(a.write(x))
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


  def partOne(i: Input): Output = {
    val initialState = State(Pos(0,0), Direction.Up, Map.empty, Command.Paint)
    val finalState = i.readProgram.run[State](initialState)
        
    log(s"viewPort=${finalState.viewPort}")
    log(finalState.message)

    finalState.hull.keySet.size.toString
  }

  def partTwo(i: Input): Output = {
    val initialState = State(Pos(0,0), Direction.Up, Map(Pos(0,0) -> Color.White), Command.Paint)
    val finalState = i.readProgram.run[State](initialState)
        
    log(s"viewPort=${finalState.viewPort}")
    
    finalState.message
  }
}