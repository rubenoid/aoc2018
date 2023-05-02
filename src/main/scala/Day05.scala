import scala.annotation.tailrec
import scala.io.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val polymer =
    Source
      .fromResource(s"input$day.txt")
      .mkString

  def destroy_units_helper(s: String): String = {
    def loop(i: Int, acc: String): String = {
      i match
        case i if i + 1 >= s.length => acc + s(i)
        case _                      =>
          if (math.abs(s(i) - s(i + 1)) != 32)
            loop(i + 1, acc + s(i))
          else
            loop(i + 2, acc)
    }
    loop(0, "")
  }

  def destroy_units(s: String): String = {
    def loop(x: String, l: Int): String = {
      l match
        case l if l == 0        => ""
        case l if l == x.length => x
        case _                  => loop(destroy_units_helper(x), x.length)
    }
    loop(destroy_units_helper(s), s.length)
  }


  val answer1: Int = destroy_units(polymer).length
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  val answer2: Int = 999

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
