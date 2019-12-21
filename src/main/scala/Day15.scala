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
    

  def partTwo(i: Input): Output =
    partOne(i)
}