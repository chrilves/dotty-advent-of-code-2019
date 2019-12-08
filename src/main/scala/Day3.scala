import scala.math._

object Day3 extends Day {
  enum Direction {
    case R,U,L,D
  }

  final case class Step(direction: Direction, length: Int)

  type Input = (List[Step], List[Step])
  type Output = Int

  def parseInput(s: String): Input = {
    val Array(first, second) =
      s.split("\n").map { line =>
        line.split(",").map { step =>
          Step(
            step.take(1) match {
              case "R" => Direction.R
              case "U" => Direction.U
              case "L" => Direction.L
              case "D" => Direction.D
            },
            step.drop(1).toInt
          )
        }.toList
      }
    
    (first, second)
  }

  sealed abstract class Segment {
    import Segment._

    def source : Point
    def destination : Point

    final def intersections(other: Segment): Set[Point] =
      (this, other) match {
        case (v1@Vertical(x1, yfrom1, yto1), v2@Vertical(x2, yfrom2, yto2)) =>
          if x1 == x2
          then {
            val ymin = max(v1.ymin, v2.ymin)
            val ymax = min(v1.ymax, v2.ymax)
            (ymin to ymax).map(Point(x1, _)).toSet
          }
          else Set.empty

        case (v1@Vertical(x, yfrom, yto), h1@Horizontal(y, xfrom, xto)) =>
          if (x >= h1.xmin) && (x <= h1.xmax) && (y >= v1.ymin) && (y <= v1.ymax)
          then Set(Point(x,y))
          else Set.empty
        
        case (h1@Horizontal(y, xfrom, xto), v1@Vertical(x, yfrom, yto)) =>
          if (x >= h1.xmin) && (x <= h1.xmax) && (y >= v1.ymin) && (y <= v1.ymax)
          then Set(Point(x,y))
          else Set.empty

        case (h1@Horizontal(y1, xfrom1, xto1), h2@Horizontal(y2, xfrom2, xto2)) =>
          if y1 == y2
          then {
            val xmin = max(h1.xmin, h2.xmin)
            val xmax = min(h1.xmax, h2.xmax)
            (xmin to xmax).map(Point(y1, _)).toSet
          }
          else Set.empty
        }
  }
  object Segment {

    final case class Vertical(x: Int, yfrom: Int, yto: Int) extends Segment {
      def ymin = min(yfrom, yto)
      def ymax = max(yfrom, yto)
      def source = Point(x, yfrom)
      def destination = Point(x, yto)
    }

    final case class Horizontal(y: Int, xfrom: Int, xto: Int) extends Segment {
      def xmin = min(xfrom, xto)
      def xmax = max(xfrom, xto)
      def source = Point(xfrom, y)
      def destination = Point(xto, y)
    }
  }

  import Segment._

  final case class Point(x: Int, y: Int) {

    def norm: Int =
      if x == 0 && y == 0
      then Int.MaxValue
      else abs(x) + abs(y)


    def distance(p: Point): Int =
      abs(x - p.x) + abs(y - p.y)

    def step(s: Step): (Segment, Point) =
      s.direction match {
        case Direction.D =>
          val newY = y - s.length
          (Vertical(x, y, newY), Point(x, newY))
        case Direction.U =>
          val newY = y + s.length
          (Vertical(x, y, newY), Point(x, newY))
        case Direction.L =>
          val newX = x - s.length
          (Horizontal(y, x, newX), Point(newX, y))
        case Direction.R =>
          val newX = x + s.length
          (Horizontal(y, x, newX), Point(newX, y))
      }
  }

  def partOne(l: Input): Output = {
    def steps2segments(origin: Point)(steps: List[Step]): Set[Segment] =
      steps match {
        case Nil => Set.empty
        case step :: tail =>
          val (segment, point) = origin.step(step)
          steps2segments(point)(tail) + segment
      }

    def intersections(steps1: List[Step], steps2: List[Step]): Set[Point] =
      for
        segment1 <- steps2segments(Point(0,0))(steps1)
        segment2 <- steps2segments(Point(0,0))(steps2)
        point    <- segment1.intersections(segment2)
      yield point
  
    intersections(l._1, l._2)
      .log("intersections")
      .minBy(_.norm)
      .log("minimal")
      .norm
  }

  def partTwo(l: Input): Output = {
    def steps2weighedSegments(origin: Point, distance: Int)(steps: List[Step]): Map[Segment, Int] =
      steps match {
        case Nil => Map.empty
        case step :: tail =>
          val (segment, point) = origin.step(step)
          val m = steps2weighedSegments(point, distance + abs(step.length))(tail)
          
          m + (segment -> (m.get(segment) match {
            case None    => distance
            case Some(d) => min(distance,d)
          }))
      }
    
    def weighedIntersection(segment1: Segment, distance1: Int, segment2: Segment, distance2: Int): Vector[(Point, Int)] =
      segment1
        .intersections(segment2)
        .toVector
        .map(p => p -> (
          distance1 + segment1.source.distance(p) +
          distance2 + segment2.source.distance(p)
        ))

    val intersections =
      for
        (segment1, distance1) <- steps2weighedSegments(Point(0,0),0)(l._1).toVector
        (segment2, distance2) <- steps2weighedSegments(Point(0,0),0)(l._2).toVector
        weighedPoint          <- weighedIntersection(segment1, distance1, segment2, distance2)
      yield weighedPoint

    val validIntersections =
      intersections
        .filterNot(_._1 == Point(0,0))
        .log("intersections")
    
    validIntersections
      .minBy(_._2)
      .log("minimal")
      ._2
  }
}