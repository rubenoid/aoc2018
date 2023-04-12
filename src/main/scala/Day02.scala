import scala.annotation.tailrec
import scala.io.*

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val ids: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList


  var twos = 0
  var threes = 0
  for id <- ids do {
    val arr = new Array[Boolean](1000)
    for (i <- 0 to 26) {
      val c = id.count(_ == 'a' + i)
      arr(c) = true
    }
    if arr(2) then twos += 1
    if arr(3) then threes += 1
  }

  val answer1: Int = twos*threes
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis
    
  def charDifference(left: String, right: String): Int = {
    def go(i: Int): Int = {
      if (i >= left.length) 0
      else if left(i) == right(i) then 0 + go(i + 1)
      else 1 + go(i + 1)
    }
    go(0)
  }

  def stringCommon(left: String, right: String): String = {
    val res = new StringBuilder("")
    for (i <- 0 until left.length) {
      if (left(i) == right(i)) then res += left(i)
    }
    res.toString()
  }

  var x: String = ""

  for (i <- ids.indices) {
    for (j <- ids.indices) {
      if (charDifference(ids(i), ids(j)) == 1) x = stringCommon(ids(i), ids(j))
    }
  }

  val answer2 = x
  println(Console.BLUE + s"Answer day $day part 1: ${answer2} [${System.currentTimeMillis - start2}ms]")