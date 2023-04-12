import scala.annotation.tailrec
import scala.io.*

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
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  var myMap = scala.collection.mutable.Set[Int]()
  var wave = 0;

  var condition = false
  while (!condition) {
    for freq <- frequencies; if !condition do {
      wave += freq
      if myMap.contains(wave) then condition = true;
      else myMap.addOne(wave)
    }
  }
  val answer2: Int = wave

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
