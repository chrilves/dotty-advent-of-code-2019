import scala.io._

enum Part {
  case One
  case Two
}

def log(s: String): Unit = println(s"${Console.YELLOW}$s${Console.RESET}")
def answer(s: String): Unit = println(s"${Console.GREEN}$s${Console.RESET}")

def [A](a:A) log(s: String): A = {
  log(s"$s=$a")
  a
}

trait Day {
  type Input
  type Output

  def parseInput(s: String): Input
  def partOne(l: Input): Output
  def partTwo(l: Input): Output

  final def run(i: String)(p: Part): Unit =
    answer(
      p match {
        case Part.One => partOne(parseInput(i)).toString
        case Part.Two => partTwo(parseInput(i)).toString
      }
    )
}

@main def main(day: Int, inputFile: String, part: Int): Unit = {
  val theday = day match {
    case 1 => Day1
    case 2 => Day2
    case 3 => Day3
    case 4 => Day4
  }

  val input = Source.fromFile(inputFile).mkString

  val thepart = part match {
    case 1 => Part.One
    case 2 => Part.Two
  }

  theday.run(input)(thepart)
}
