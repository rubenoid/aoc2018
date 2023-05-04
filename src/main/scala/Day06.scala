import scala.+:
import scala.annotation.tailrec
import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

//  val pattern = """(\d+), (\d+)""".r
  val coordinates =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$x, $y" => (x.toInt, y.toInt) }
      .toList

  println(coordinates)

  val dim = 10
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

  /* Rewriting the following using map

    for (i <- field.indices)
      for (j <- field(0).indices)
          val indexPoint = indexToClosestPoint(i, j, coordinates)
          if (indexPoint == -1)
            field(i)(j) = -1
          else
            field(i)(j) = indexPoint

  */
  field = field.zipWithIndex
    .map(row => row._1
      .zipWithIndex
      .map(e => {
        val indexPoint = indexToClosestPoint(row._2, e._2, coordinates)
        if (indexPoint == -1) -1 else indexPoint
      }))

  def findMaxPoints(): Int = {
    val setOfOutsideEl: Set[Int] = (
      field.head ++
        field.last ++
        field.transpose.head ++
        field.transpose.last)
      .toSet

    field.flatten
      .filter(x => !setOfOutsideEl.contains(x)) // filter the elements that have a member on the outside
      .filter(_ >= 0) // filter the -1's
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
    points.foldLeft(0)((acc, point) => {
      val dist = manhattenDist(x1, y1, point._1, point._2)
      acc + dist
    })

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
