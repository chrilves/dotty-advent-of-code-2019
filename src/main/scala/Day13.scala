import scala.math.{log => _, _}
import scala.annotation._

object Day13 extends Day {
  type Input = Array[BigInt]
  type Output = String

  def parseInput(s: String): Input =
    s.split(",").map(BigInt(_))

  enum Tile {
    case Empty, Wall, Block, HorizontalPaddle, Ball
  }

  final case class Pos(x: Int, y: Int) {
    override def toString = s"($x,$y)"
  }

  trait StateIO[A] {
    def (a:A) read : (BigInt, A)
    def (a:A) write(x: BigInt): A
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

  def (input: Array[BigInt]) readProgram: Program = {
    import Program._

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

  def (o: BigInt) tile: Tile =
    o.toInt match {
      case 0 => Tile.Empty
      case 1 => Tile.Wall
      case 2 => Tile.Block
      case 3 => Tile.HorizontalPaddle
      case 4 => Tile.Ball
    }

  def partOne(i: Input): Output = {
    final case class Event(pos: Pos, tile: Tile)
    final case class State(events: Vector[Event], stack: List[BigInt])
  
    given stateIO : StateIO[State] {
      def (state:State) read : (BigInt, State) = throw new Exception("No input")
  
      def (state:State) write(o: BigInt): State =
        state.stack match {
          case y :: x :: Nil =>
            State(state.events :+ Event(Pos(x.toInt,y.toInt), o.tile), List.empty)
          case _             =>
            State(state.events, o :: state.stack)
        }
    }  

    val initialState = State(Vector.empty, List.empty)
    val finalState = i.readProgram.run[State](initialState)

    val blocks =
      finalState.events.foldLeft(Set.empty[Pos]) {
        case (s, e) => e.tile match {
          case Tile.Block => s + e.pos
          case _          => s - e.pos
        }
      }

    blocks.size.toString
  }

  def partTwo(i: Input): Output = {
    i(0) = BigInt(2)

    final case class ViewPort(minX: Int, maxX: Int, minY: Int, maxY: Int) {
      def width = maxX - minX + 1
      def height = maxY - minY + 1
    }

    final case class BallInfo(pos: Pos, up: Boolean, left: Boolean) {
      def update(p: Pos): BallInfo = BallInfo(p, p.y > pos.y, p.x < pos.x)
      def message: String = {
        val u = if up then "U" else "D"
        val l = if left then "L" else "R"
        s"Ball $pos $u $l"
      }
    }
  
    final case class State(
      tiles: Map[Pos, Tile],
      ball: BallInfo,
      paddle: Pos,
      score: BigInt,
      stack: List[BigInt]) {
  
      val viewPort: ViewPort  =
        tiles.keysIterator.foldLeft(ViewPort(0,0,0,0)) {
          case (v, p) => ViewPort(min(p.x, v.minX), max(p.x, v.maxX), min(p.y, v.minY), max(p.y, v.maxY))
        }
  
      def screen: String =
        Array.tabulate(viewPort.height, viewPort.width) { (y,x) =>
          tiles.getOrElse(Pos(x + viewPort.minX, viewPort.maxY - y), Tile.Empty) match {
            case Tile.Empty => " "
            case Tile.Block => "X"
            case Tile.Wall  => "#"
            case Tile.HorizontalPaddle => "="
            case Tile.Ball => "o"
          }
        }.map(_.mkString).mkString("\n")
    }

    given stateIO : StateIO[State] {
      def (state:State) read : (BigInt, State) = {
        println("")
        log(state.screen)
        log(s"Score=${state.score}")
        log(state.ball.message)
        log(s"paddle at ${state.paddle}")
        
        val move: Int =
          if state.ball.pos.y == state.paddle.y - 1 &&
             state.ball.pos.x == state.paddle.x 
          then 0
          else {
            val nextBallX =
              state.ball.pos.x + (if state.ball.left then -1 else 1)
            
            if nextBallX == state.paddle.x
            then 0
            else if nextBallX > state.paddle.x
                 then 1
                 else -1
          }

        log(s"move: $move")
        (BigInt(move), state)
        /*
        
        print("Where to move (q,s,d): ")
        scala.io.StdIn.readChar match {
          case 'q' => (BigInt(-1), state)
          case 's' => (BigInt( 0), state)
          case 'd' => (BigInt( 1), state)
          case _   => state.read
        } */
      }
          
      def (state:State) write(o: BigInt): State =
        state.stack match {
          case 0 :: (-1) :: Nil =>
            State(
              state.tiles,
              state.ball,
              state.paddle,
              o,
              List.empty
            )
          case y :: x    :: Nil =>
            val pos  = Pos(x.toInt,y.toInt)
            val tile = o.tile
            val newBall =
              tile match {
                case Tile.Ball => state.ball.update(pos)
                case _         => state.ball
              }
            val newPaddleX =
              tile match {
                case Tile.HorizontalPaddle => pos
                case _                     => state.paddle
              }
            State(
              state.tiles + (pos -> tile),
              newBall,
              newPaddleX,
              state.score,
              List.empty
            )
          case _ =>
            State(
              state.tiles,
              state.ball,
              state.paddle,
              state.score,
              o :: state.stack
            )
        }
    }

    val initialState = State(Map.empty, BallInfo(Pos(0,0), true, true), Pos(0,0), 0, List.empty)
    val finalState = i.readProgram.run[State](initialState)

    finalState.score.toString
  
  }
}