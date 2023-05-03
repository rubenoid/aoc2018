

import scala.annotation.tailrec
import scala.io.*
import scala.language.postfixOps

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  var polymer =
    Source
      .fromResource(s"input$day.txt")
      .mkString

  /*
  Make a list with all possible units
    Return: List(('a','A'),('A','a'),('b','B'),...
  */
  val lowercase = ('a' to 'z').toList
  val uppercase = ('A' to 'Z').toList
  val table = lowercase.zip(uppercase).flatMap{
    case (i, j) => Seq(s"$i$j", s"$j$i")
  }

  /*
  Replace string s with replacement string for all strings in List t
   */
  def replaceStrings(s: String, t: List[String], replacement: String): String = {
    t.foldLeft(s)((curLine, str) => curLine.replace(str, replacement))
  }

  /*
  Apply function f on s and continue while the result of f(s) is shorter then the length of s
  */
  def destroy_units(s: String)(f: (String, List[String], String) => String, t: List[String], r: String): String = {
    def loop(x: String, l: Int): String = {
      l match
        case l if l == 0 => ""
        case l if l == x.length => x
        case _                  => loop(f(x, t, r), x.length)
    }
    loop(f(s, t, r), s.length)
  }

  val new_polymer = destroy_units(polymer)(replaceStrings, table, "")

  val answer1 = new_polymer.length
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  /*
  First adjust the polymer (tmpPol) and then destroy_units and put the resulting tmpNewPol in a list of strings
  */
  def adjustedPolymersResult(s: String, t: List[String]): List[String] = {
    def loop(x: String, acc: List[String], i: Int): List[String] = {
      i match {
        case i if i == 26 => acc
        case _ =>
          val tmpPol = polymer.filter(x => x != (65 + i).toChar && x != (97 + i).toChar)
          val tmpNewPol = destroy_units(tmpPol)(replaceStrings, t, "")
          loop(x, acc :+ tmpNewPol, i + 1)
      }
    }
    loop(s, List[String](), 0)
  }

  val adjusted =  adjustedPolymersResult(polymer, table)
  val answer2 = adjusted.minBy(x => x.length).length

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
