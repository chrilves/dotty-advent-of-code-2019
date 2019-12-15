import scala.math.{log => _, _}
import scala.annotation._

object Day14 extends Day {
  final case class Mat(name: String) {
    override def toString: String = name
  }
  final case class Recipe(ingredients: Map[Mat, Long], product: Mat, quantity: Long) {
    override def toString : String = {
      val ingStr = ingredients.map { case (k,v) => s"$v $k"}.mkString(", ")
      s"$quantity $product <= $ingStr"
    }
  }

  type Input = Map[Mat, Recipe]
  type Output = String

  def parseInput(s: String): Input =
    s.split("\n").map { line =>
      val Array(ingreds,prod) = line.split("=>")
      val Array(quantity, product) = prod.trim.split(" ")
      val ingedients =
        ingreds.split(",").map { ingr =>
          val Array(quant, mat) = ingr.trim.split(" ")
          Mat(mat) -> quant.toLong
        }.toMap
      Mat(product) -> Recipe(ingedients, Mat(product), quantity.toLong)
    }.toMap

  final class Ops(i: Input) {
    val d = scala.collection.mutable.Map.empty[Mat, Set[Mat]]

    def dependsOn(prod: Mat): Set[Mat] =
      d.getOrElseUpdate(prod,
        i.get(prod) match {
          case Some(Recipe(ingreds,_,_)) =>
            ingreds.keySet.flatMap(n => dependsOn(n) + n)
          case _ =>
            Set.empty
        }
      )
    
    val fuel = Mat("FUEL")
    val ore  = Mat("ORE")
    
    given prodOrd : Ordering[Mat] {
      def compare(m1: Mat, m2: Mat): Int =
        if dependsOn(m1).contains(m2)
        then -1
        else if dependsOn(m2).contains(m1)
             then 1
             else Ordering[String].compare(m1.name,m2.name)
    }

    def rootsOf(mats: Set[Mat]): Set[Mat] =
      mats.foldLeft(mats.intersect(i.keySet)) {
        case (s,m) => s -- dependsOn(m)
      }

    inline def ceil(a: Long, b: Long): Long =
      (a / b) + (if a % b == 0 then 0 else 1)

    def need(mats: Map[Mat, Long], loop : Boolean = true): Map[Mat, Long] =
      if mats.isEmpty
      then mats
      else {
        val roots = rootsOf(mats.keySet)

        //log(s"Roots=${roots.mkString(",")}")
        
        if roots.isEmpty
        then mats
        else {
          val rest : Map[Mat, Long] = mats -- roots

          val reactions : List[(Recipe, Long)]=
            roots.toList.map { r =>
              val nb = ceil(mats(r), i(r).quantity)
              i(r) -> nb
            }
          
          /*log("====================================")
          reactions.foreach { case (r,n) =>
            log(s"$n * [$r]")
          }*/

          val needMore: List[(Mat, Long)] =
            for
              (react, nb) <- reactions
              (mat, qty)  <- react.ingredients.toList              
            yield mat -> nb * qty
          
          val needed: Map[Mat, Long] =
            (rest.toList ++ needMore)
              .groupBy(_._1)
              .view.mapValues(_.map(_._2).sum)
              .toMap
            
          if loop
          then need(needed, loop)
          else needed
        }
      }
  }

  def partOne(i: Input): Output = {
    val ops = Ops(i)
    import ops.{given, _}
    val required = need(Map(fuel -> 1))
    log(s"required=$required")
    required(ore).toString
  }
    

  def partTwo(i: Input): Output = {
    val ops = Ops(i)
    import ops.{given, _}

    val target: Long = 1000000000000L

    def oreForFuel(n: Long): Long =
      need(Map(fuel -> n))(ore)
    
    def findMoreThanTarget(n: Long): Long =
      if oreForFuel(n) <= target
      then findMoreThanTarget(2 * n)
      else n
    
    def dichotomy(from: Long, to: Long): Long =
      if from + 1 == to
      then from
      else {
        val mid = (from + to) / 2
        if oreForFuel(mid) <= target
        then dichotomy(mid, to)
        else dichotomy(from, mid)
      }
    
    dichotomy(1, findMoreThanTarget(1)).toString
  }
}