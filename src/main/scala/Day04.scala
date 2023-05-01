import scala.annotation.tailrec
import scala.io.*
import java.io.{File, FileWriter, BufferedWriter}

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val records: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .sorted

//  def writeFile(filename: String, lines: Seq[String]): Unit = {
//    val file = new File(filename)
//    val bw = new BufferedWriter(new FileWriter(file))
//    for (line <- lines) {
//      bw.write(line + '\n')
//    }
//    bw.close()
//  }

//  writeFile("test.txt", records)

  val pattern_guard = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift""".r

  val pattern_wakes_up = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] wakes up""".r
  val pattern_falls_asleep = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] falls asleep""".r

  val guards: collection.mutable.Map[Int, Int] = (for {
    r <- records
    m <- pattern_guard.findAllMatchIn(r)
  } yield m.group(6).toInt).
    map(x => (x, 0)).
    to(collection.mutable.Map)

  var current_guard = 0
  var start = 0
  for (r <- records) {
    r match
      case pattern_guard(_,_,_,_,_, id)           => current_guard = id.toInt
      case pattern_falls_asleep(_,_,_,_, minute)  => start = minute.toInt
      case pattern_wakes_up(_,_,_,_, minute)      =>
        guards(current_guard) += minute.toInt - start
      case _                                      => None
  }

  val max_guard_id = guards.maxBy(_._2)._1
  val arrMinutes = new Array[Int](60)
  for (r <- records) {
    r match
      case pattern_guard(_,_,_,_,_, id) => current_guard = id.toInt
      case pattern_falls_asleep(_, _, _, _, minute)
        if current_guard == max_guard_id
      => start = minute.toInt
      case pattern_wakes_up(_, _, _, _, minute)
        if current_guard == max_guard_id
      => for (i <- start until minute.toInt)
          arrMinutes(i) += 1
      case _ => None
  }

  val minute_most_asleep = arrMinutes.indexOf(arrMinutes.max)
  val answer1: Int = minute_most_asleep * max_guard_id
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  val answer2: Int = 999

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
