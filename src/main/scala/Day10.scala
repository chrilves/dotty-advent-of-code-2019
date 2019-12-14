import scala.math.{log => _, _}
import scala.annotation._

object Day10 extends Day {
  type Input  = Array[Array[Boolean]]
  type Output = String

  def parseInput(s: String): Input = 
    s.split("\n").map(_.toArray.map {
      case '#' => true
      case '.' => false
    })
    
  import scala.math.abs

  def pgcd(a: Int, b: Int): Int =
    if a < 0 || b < 0
    then pgcd(abs(a), abs(b))
    else if b > a
          then pgcd(b,a)
          else if b == a || b == 0
              then a
              else pgcd(b, a % b)
  
  object Types {
    final case class Vec2D(x: Int, y: Int) {
      def uvec2d: UVec2D = UVec2D(x,y)
      inline def -(v: Vec2D) : Vec2D = Vec2D(x - v.x, y - v.y)
      inline def norm: Int = x*x + y*y
    }

    opaque type UVec2D = Vec2D

    object UVec2D {
      def apply(x: Int, y: Int): UVec2D =
        if x == 0 && y == 0
        then Vec2D(0,0)
        else {
          val d = pgcd(x,y)
          Vec2D(x / d, y / d)
        }

      given uvec2dOrdering : Ordering[UVec2D] {
        def compare(u1: UVec2D, u2: UVec2D): Int = {
          inline def sign(z: Int): Int =
            if z < 0 then -1 else if z > 0 then 1 else 0

          inline def secondHalf(p: UVec2D): Int =
            if p.x >= 0 then 0 else 1
          
          if u2.x == 0 && u1.x == 0
          then sign(u1.y - u2.y)
          else (secondHalf(u1) - secondHalf(u2) match {
            case 0 => sign((u1.y * u2.x) - (u2.y * u1.x))
            case r => r
          })
        }
      }
    }
  }

  import Types._

  final case class Pos(x: Int, y: Int) {
    def to(p: Pos): Vec2D = Vec2D(p.x - x, p.y - y)
    def dir(p: Pos): UVec2D = UVec2D(p.x - x, p.y - y)
  }


  def best(i: Input): (Pos, Int) = {
    val w : Int = i(0).size
    val h : Int = i.size

    inline def asteroidIn(p: Pos): Boolean = i(p.y)(p.x)

    def see(p: Pos): Int = {
      val seen = scala.collection.mutable.TreeSet.empty[UVec2D](given UVec2D.uvec2dOrdering)

      for
        x <- 0 to (w - 1)
        y <- 0 to (h - 1)
      do {
        val pa = Pos(x,y)
        if pa != p && asteroidIn(pa)
        then seen += p.dir(pa)
      }

      seen.size
    }

    log(s"w=$w, h=$h")
    log(s"Asteroids = ${i.map(_.filter(x => x).size).sum}")

    val ateroids : Stream[(Pos, Int)] =
      for
        x <- (0 to (w - 1)).toStream
        y <- (0 to (h - 1)).toStream
        p = Pos(x,y)
        if asteroidIn(p)
      yield p -> see(p)
    
    ateroids.maxBy(_._2)
  }

  def partOne(i: Input): Output = {
    val w : Int = i(0).size
    val h : Int = i.size

    val b = best(i)
    log(b.toString)

    b._2.toString
  }

  def partTwo(i: Input): Output = {
    val w : Int = i(0).size
    val h : Int = i.size

    val b = best(i)
    log(b.toString)

    val station = b._1

    inline def asteroidIn(p: Pos): Boolean = i(p.y)(p.x)

    val ateroids : List[(Pos, Vec2D)] =
      for
        x <- (0 to (w - 1)).toList
        y <- (0 to (h - 1)).toList
        p = Pos(x,y)
        if asteroidIn(p) && p != station
      yield p -> station.to(p)
    
    final class Result(pos: Pos, dir: UVec2D, index: Int)
    
    def key(z : (Pos, Vec2D)): UVec2D = z._2.uvec2d

    val z : Int =
      ateroids.groupBy(_._2.uvec2d)
    
    ""
  }
}