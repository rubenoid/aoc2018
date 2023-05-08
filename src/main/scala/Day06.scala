import scala.annotation.tailrec
import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Coordinate(x: Int, y: Int)

  case class Grid(coordinates: List[Coordinate]):
    private val maxX: Int = coordinates.maxBy(_.x).x
    private val maxY: Int = coordinates.maxBy(_.y).y
    private val border: Int = if (maxX > maxY) maxX else maxY

    private val xy: List[Coordinate] = (for {
      i <- 0 to border
      j <- 0 to border
    } yield Coordinate(i, j)).toList

    private val pointsWithClosestTo: List[(Coordinate, Int)] = xy.map(p => (p, indexToClosestPoint(p)))

    private def manhattanDistance(coord1: Coordinate, coord2: Coordinate): Int =
      import math.abs
      abs(coord1.x - coord2.x) + abs(coord1.y - coord2.y)

    /* TODO Option[Int */
    private def indexToClosestPoint(point: Coordinate): Int =
      val distances: List[Int] = for {
        coord <- coordinates
        dist = manhattanDistance(point, coord)
      } yield dist
      val min: Int = distances.min
      if (distances.count(x => x == min) > 1) -1 else distances.indexOf(min)

    /*
      The infinite points, i.e. the points that are on the border, should be removed from the list.
      Result is a list of valid points.
    */
    private def validPoints: List[(Coordinate, Int)] =
      val setOfInfinites =  pointsWithClosestTo.filter {
        case (p, _) => p.x == 0 || p.x == border || p.y == 0 || p.y == border
      }
        .map(_._2)
        .toSet

      pointsWithClosestTo.filterNot {
        case (_, v) => setOfInfinites.contains(v)
      }
//      pointsWithClosestTo.filterNot(p => setOfInfinites.contains(p._2))

    /* Part 1 */
    def getMost: Int =
      validPoints
        .map(x => x._2)
        .groupBy(identity)
        .map(x => x._2.length)
        .max

    /* Part 2 */
    def distanceToPoints(point: Coordinate): Int =
      coordinates.foldLeft(0)((acc, coord) => {
        val dist = manhattanDistance(point, coord)
        acc + dist
      })

    def amountRegionPoints: Int =
      xy.indices.foldLeft(0)((acc, i) => {
      val dist = distanceToPoints(xy(i))
      if (dist < 10000) acc + 1 else acc
    })

  end Grid

  val coordinates: List[Coordinate] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$x, $y" => Coordinate(x.toInt, y.toInt) }
      .toList


  val answer1: Int = Grid(coordinates).getMost
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis


  val answer2: Int = Grid(coordinates).amountRegionPoints

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
