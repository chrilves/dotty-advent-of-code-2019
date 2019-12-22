package dottyaoc

import scala.math._

object Day1 extends Day {
  type Input = List[Long]
  type Output = Long

  def parseInput(s: String): List[Long] =
    s.split("\n").map(_.toLong).toList

  def partOne(l: List[Long]): Long =
    l.map(i => max(i / 3 - 2, 0)).sum

  def partTwo(l: List[Long]): Long = {
    def aux(n : Long): Long = {
      val fuel = n / 3 - 2

      if fuel <= 0
      then 0
      else fuel + aux(fuel)
    }

    l.map(aux).sum
  }
}