import scala.+:
import scala.annotation.tailrec
import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input =
    Source
      .fromResource(s"input$day.txt")
      .getLines

  val pattern = """(\d+), (\d+)""".r

  val coordinates = {
    var tmp: List[(Int, Int)] = List[(Int, Int)]()
    for (l <- input) {
      l match
        case pattern(x, y) =>
          val t = (y.toInt, x.toInt)
          tmp = tmp :+ t
    }
    tmp
  }
  println(coordinates)

  val dim = 400
  var field = Array.fill[Int](dim,dim)(-2)

  def manhattenDist(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    math.abs(x1 - x2) + math.abs(y1 - y2)
  }

  /*
  Returns the index of the closest point out of the list points
  -1 if there are more than one point that is closest
  */
  def indexToClosestPoint(x1: Int, y1: Int, points: List[(Int, Int)]): Int = {
    val distances = for {
      point <- points
      dist = manhattenDist(x1, y1, point._1, point._2)
    } yield dist
    val minimum = distances.min
    val amountOfPoints = distances.count(x => x == minimum)
    if (amountOfPoints > 1)
      -1
    else
      distances.indexOf(distances.min)
  }

  def pointIsInList(x1: Int, y1: Int, points: List[(Int, Int)]): Boolean = {
    points.contains((x1, y1))
  }

  for (i <- field.indices)
    for (j <- field(0).indices)
//      if (!pointIsInList(i, j, coordinates))
        val indexPoint = indexToClosestPoint(i, j, coordinates)
        if (indexPoint == -1)
          field(i)(j) = -1
        else
          field(i)(j) = indexPoint

  def findMaxPoints(): Int = {
    var keys = for {
      i <- 1 until field.length
      j <- 1 until field.length
      key = field(i)(j)
    } yield(key)

    for (i <- field.indices)
      val l = field.length
      keys = keys.filter(x => {
        x != field(i)(0) &&
          x != field(i)(l - 1) &&
          x != field(0)(i) &&
          x != field(l - 1)(i)
      })
    keys.filter(_ >= 0)
      .groupBy(identity)
      .map(_._2.length)
      .max

  }
  println(findMaxPoints())

  val answer1: Int = findMaxPoints()
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  def distanceToPoints(x1: Int, y1: Int, points: List[(Int, Int)]): Int =
    val distances = for {
      point <- points
      dist = manhattenDist(x1, y1, point._1, point._2)
    } yield dist
    distances.sum

  val amountRegionPoints = {
    val distancePoints = for {
      i <- 0 until dim
      j <- 0 until dim
      dist = distanceToPoints(i, j, coordinates)
    } yield dist
    distancePoints.count(_ < 10000)
  }

  val answer2: Int = amountRegionPoints

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
