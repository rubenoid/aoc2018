import scala.io.*

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val instructions =
    Source
      .fromResource(s"input$day.txt")
      .getLines()

  println(instructions)
  val answer1: String = "AAA"
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  val answer2: String = "AAA"

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
