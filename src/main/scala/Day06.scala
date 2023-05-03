import scala.annotation.tailrec
import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val coordinates: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  println(coordinates)

  val answer1: Int = 0
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  val answer2: Int = 0

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
