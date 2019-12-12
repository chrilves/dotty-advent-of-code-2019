import scala.math.{log => _, _}
import scala.annotation._

object Day8 extends Day {
  type Input = List[Char]
  type Output = String

  def parseInput(s: String): Input = 
    s.toList

  def partOne(i: Input): Output = {
    val layer = i.grouped(25*6).minBy(_.filter(_ == '0').size)
    (layer.filter(_ == '1').size * layer.filter(_ == '2').size).toString
  }
    

  def partTwo(i: Input): Output = {
    val layers = i.toArray.grouped(25*6).toList
    val result = Array.tabulate(25*6) { pos =>
      layers.foldRight('2') { case (arr, st) =>
        if arr(pos) == '2'
        then st
        else arr(pos)  
      }
    }
    result.map {
      case '0' => ' '
      case '1' => 'o'
      case _   => '?'
    }.grouped(25).map(_.mkString).mkString("\n")
  }
}