import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val frequencies: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  val answer1: Int = frequencies.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
