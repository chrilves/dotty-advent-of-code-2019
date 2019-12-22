package dottyaoc

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

  enum Dir {
    case North, South, East, West
  }

  enum Status {
    case Wall, Ok, Oxygen
  }

  def partOne(i: Input): Output = {
    /*
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
    */
    ""
  }
    

  def partTwo(i: Input): Output = {
    i(0) = BigInt(2)


    enum Move {
      case Left, Stay, Right
    }

    enum Tile {
      case Empty, Wall, Block, HorizontalPaddle, Ball
    }

    enum Print {
      case TilePos(pos: Pos, tile: Tile)
      case Score(score: BigInt)
    }

    import IntCode2._

    def outputDecoder: Process[BigInt, Print, Nothing] =
      for
        x <- Process.input[BigInt]
        y <- Process.input[BigInt]
        t <- Process.input[BigInt]
        _ <- Process.output {
              (x,y) match {
                case (-1, 0) => Print.Score(t)
                case _ =>
                  val pos  = Pos(x.toInt,y.toInt)
                  val tile =
                    t match {
                      case 0 => Tile.Empty
                      case 1 => Tile.Wall
                      case 2 => Tile.Block
                      case 3 => Tile.HorizontalPaddle
                      case 4 => Tile.Ball
                    }
                  Print.TilePos(pos, tile)
                }
             }
        r <- outputDecoder
      yield r

    val prg =
      Process.unsafeFromProgram(i)  

    val program =
      prg.post(outputDecoder).mapIO(
        (i: Move) => i match {
          case Move.Left  => -1
          case Move.Stay  => 0
          case Move.Right => 1
        },
        identity[Print]
      )

  
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
      iterations: Long) {
  
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

  
    given stateIO : StateIO[Move, Print, State] {
      def (state:State) read : (Move, State) = {
        
        log(s"""
${state.screen}
Score=${state.score}
${state.ball.message}
paddle at ${state.paddle}
iteration ${state.iterations}
""")
        
        val move: Move =
          if state.ball.pos.y == state.paddle.y - 1 &&
             state.ball.pos.x == state.paddle.x 
          then Move.Stay
          else {
            val nextBallX =
              state.ball.pos.x + (if state.ball.left then -1 else 1)
            
            if nextBallX == state.paddle.x
            then Move.Stay
            else if nextBallX > state.paddle.x
                 then Move.Right
                 else Move.Left
          }

        //log(s"move: $move")
        (move, state.copy(iterations = state.iterations + 1))
      }
          
      def (state:State) write(o: Vector[Print]): State = {
        
        val (newScore, newBall, newPaddle) =
          o.foldRight((state.score, state.ball, state.paddle)) {
            case (Print.Score(s)                           ,(score, ball, paddle)) => (s    ,ball            , paddle)
            case (Print.TilePos(pos, Tile.Ball)            ,(score, ball, paddle)) => (score,ball.update(pos), paddle)
            case (Print.TilePos(pos, Tile.HorizontalPaddle),(score, ball, paddle)) => (score,ball            , pos   )
            case (_, acc) => acc
          }
        
    
        State(
          o.foldRight(state.tiles) {
            case (Print.TilePos(p,t),m) => m + (p -> t)
            case (_,m) => m
          },
          newBall,
          newPaddle,
          newScore,
          state.iterations
        )
      }
    }

    val initialState = State(Map.empty, BallInfo(Pos(0,0), true, true), Pos(0,0), 0,0)
    val finalState = program.run[State](initialState)

    finalState._2.score.toString
  }
}