import scala.math.{log => _, _}
import scala.annotation._

object Day8 extends Day {
  type Input = List[Char]
  type Output = Int

  def parseInput(s: String): Input = 
    s.toList

  def partOne(i: Input): Output = {
    val layer = i.grouped(25*6).minBy(_.filter(_ == '0').size)
    layer.filter(_ == '1').size * layer.filter(_ == '2').size
  }
    

  def partTwo(i: Input): Output =
    partOne(i)
   
}