package dottyaoc

import scala.math.{log => _, _}
import scala.annotation._

object Day12 extends Day {

  sealed abstract class Position
  sealed abstract class Velocity
  sealed abstract class Gravity

  inline def sign(x: Int): Int = if x > 0 then 1 else if x < 0 then -1 else 0

  final case class Vec3[A](x: Int, y: Int, z: Int) {
    inline def +(v: Vec3[A]): Vec3[A] = Vec3[A](x + v.x, y + v.y, z + v.z)
    inline def as[B]: Vec3[B] = Vec3[B](x,y,z)

    def energy : Int = abs(x) + abs(y) + abs(z)

    def gravity(p: Vec3[Position])(given A =:= Position): Vec3[Gravity] =
      Vec3[Gravity](sign(p.x - x), sign(p.y - y), sign(p.z - z))

    def gravities(l: List[Vec3[Position]])(given A =:= Position): Vec3[Gravity] =
      l.map(p => gravity(p)).foldLeft(Vec3[Gravity](0,0,0))(_ + _) 
  }

  final case class Moon(position: Vec3[Position], velocity: Vec3[Velocity]) {
    def update(g: Vec3[Gravity]): Moon = {
      val newVelocity = velocity + g.as[Velocity]
      Moon(position + newVelocity.as[Position], newVelocity)
    }

    def energy : Int = position.energy * velocity.energy

    def X: Dim[X] = Dim(position.x, velocity.x)
    def Y: Dim[Z] = Dim(position.y, velocity.y)
    def Z: Dim[Z] = Dim(position.z, velocity.z)
  }

  type Input = List[Moon]
  type Output = BigInt

  def parseInput(s: String): Input = 
    s.split("\n").toList.map { s =>
      val pat = "<x=(-?[0-9]+),y=(-?[0-9]+),z=(-?[0-9]+)>".r

      s.filterNot(_.isSpaceChar).toLowerCase match {
        case pat(x,y,z) => Moon(Vec3[Position](x.toInt,y.toInt,z.toInt), Vec3[Velocity](0,0,0))
      }
    }

  given systemOps : (l: List[Moon]) extended with {
    inline def oneStep: List[Moon] =
      l.map { moon =>
        moon.update(moon.position.gravities(l.map(_.position)))
      }
    
    @tailrec
    def steps(n: Long): List[Moon] =
      if n <= 0
      then l
      else l.oneStep.steps(n - 1)

    inline def energy: Int =
      l.map(_.energy).sum

    inline def same(l2: List[Moon]): Boolean = {
      def aux(s1: List[Moon], s2: List[Moon]): Boolean =
        (s1, s2) match {
          case (hd1 :: tl1, hd2 :: tl2) => if hd1.position != hd2.position
                                           then false
                                           else aux(tl1, tl2)
          case (Nil, Nil) => true
          case (_,_) => false
        }
      
      aux(l, l2)
    }

  } 

  def partOne(i: Input): Output = {
    val end = i.steps(1000)
    end.foreach(m => log(m.toString))
    BigInt(end.energy)
  }
   
  final abstract class X
  final abstract class Y
  final abstract class Z

  final case class Dim[A](pos: Int, vel: Int) {
    inline def update(l: List[Dim[A]]): Dim[A] = {
      val newVel = l.foldLeft(vel) { case (v, d) => v + sign(d.pos - pos) }
      Dim(pos + newVel, newVel)
    }
  }

  given dimOps : [A](l: List[Dim[A]]) extended with {
    inline def oneStep: List[Dim[A]] =
      l.map(_.update(l))
    
    inline def steps(n: Long): List[Dim[A]] =
      if n <= 0
      then l
      else l.oneStep.steps(n - 1)

    inline def same(l2: List[Dim[A]]): Boolean = {
      def aux(s1: List[Dim[A]], s2: List[Dim[A]]): Boolean =
        (s1, s2) match {
          case (hd1 :: tl1, hd2 :: tl2) => if hd1.pos != hd2.pos
                                          then false
                                          else aux(tl1, tl2)
          case (Nil, Nil) => true
          case (_,_) => false
        }
    
      aux(l, l2)
    }

    def period: (Long, Long) = {
      var state = l
      val seen  = scala.collection.mutable.Map(state -> 0L)
      var iter  = 0L
  
      while {
          state = state.oneStep
          iter += 1
          !seen.contains(state)    
      } do {
        if iter % 1000000 == 0 then log(s"iter=${iter / 1000000}")
        seen.update(state, iter)
      }
      val first = seen(state)
      (first, iter - first)
    }
  } 
  
  def partTwo(i: Input): Output = {
    val dimX = i.map(_.X)
    val dimY = i.map(_.Y)
    val dimZ = i.map(_.Z) 
    
    val (firstX, periodX) = dimX.period
    val (firstY, periodY) = dimY.period
    val (firstZ, periodZ) = dimZ.period

    log(s"firstX=$firstX, periodX=$periodX")
    log(s"firstY=$firstY, periodY=$periodY")
    log(s"firstZ=$firstX, periodZ=$periodZ")


    val firstAll  = List(firstX, firstY, firstZ).max
    val periodAll = List(periodX, periodY, periodZ).foldLeft(BigInt(1)) {
      case (ppcm, x) =>
        val bx = BigInt(x)
        (ppcm * x) / ppcm.gcd(x) 
    }

    log(s"firstAll=$firstAll, periodAll=$periodAll")

    periodAll + firstAll
  }
}