import scala.math._
import scala.annotation._

object Day6 extends Day {
  type Input = List[(String, String)]
  type Output = Int

  def parseInput(s: String): Input = 
    s.split("\n").toList.map { line =>
      val Array(left, right) = line.split("""[)]""")
      (left, right)
    }

  def partOne(i: Input): Output = {
    val orbitAround: Map[String, Set[String]] =
      i.groupBy(_._2).view.mapValues(_.map(_._1).toSet).toMap

    val cache: scala.collection.mutable.Map[String, Int] =
      scala.collection.mutable.Map.empty[String, Int]

    def count(obj: String): Int =
      cache.getOrElseUpdate(obj, {
        orbitAround.get(obj) match {
          case Some(set) => set.map(o => count(o) + 1).sum
          case _ => 0
        }
      })
    
    orbitAround.keys.toList.map(count).sum
  }


  def partTwo(i: Input): Output = {
    val orbitAround: Map[String, Set[String]] =
      i.groupBy(_._2).view.mapValues(_.map(_._1).toSet).toMap
    
    val isOrbitedBy: Map[String, Set[String]] =
      i.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    
    val connections: Map[String, Set[String]] =
      (orbitAround.keySet ++ isOrbitedBy.keySet).map { k =>
        k -> ((orbitAround.getOrElse(k, Set.empty) ++ isOrbitedBy.getOrElse(k, Set.empty)) - k)
      }.toMap
    
    val distance: scala.collection.mutable.Map[String, Int] =
      scala.collection.mutable.Map.empty[String, Int]
    
    def walk(obj: String, d: Int): Unit =
      distance.get(obj) match {
        case Some(d2) if d2 <= d => ()
        case _ =>
          distance.update(obj,d)
          for
            o <- connections.getOrElse(obj, Set.empty)
          do walk(o, d + 1)
      }
    
    walk("YOU", 0)
    distance("SAN") - 2
  }
    
}