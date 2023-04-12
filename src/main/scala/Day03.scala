import scala.annotation.tailrec
import scala.io.*
import util.control.Breaks._


object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val claims: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList


  val arr = Array.ofDim[Int](1000, 1000)
  case class Rect(id: Int, left: Int, top: Int, width: Int, height: Int)
  val pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  /* Part one solution using a 2D Array of Ints */
  for (claim <- claims) {
    val rect = claim match {
      case pattern(id, left, top, width, height) =>
        Rect(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
    }

    for {
      i <- rect.top until rect.top + rect.height
      j <- rect.left until rect.left + rect.width
    } {
      if (arr(i)(j) != 0) arr(i)(j) = -1
      else arr(i)(j) = rect.id
    }
  }

  val answer1 = (for {
    i <- arr.indices
    count = arr(i).count(_ == -1)
  }yield count).sum

  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis
  var answer2: Int = 0
  val flattenendArr = arr.flatten

  for (claim <- claims) {
    val rect = claim match {
      case pattern(id, left, top, width, height) =>
        Rect(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
    }
    val findSq = rect.width * rect.height
    if (flattenendArr.count(_ == rect.id) == findSq) answer2 = rect.id
  }

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

