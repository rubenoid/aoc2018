import scala.collection.immutable.Queue
import scala.io.*

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Step(left: Char, right: Char)

  object Step:
    implicit val stepOrdering: Ordering[Step] =
      new Ordering[Step]:
        override def compare(x: Step, y: Step): Int =
          val compareFirst = x.left - y.left
          if (compareFirst == 0) x.right - y.right else compareFirst

  val inputSteps =
    Source
      .fromResource(s"input$day.txt")
      .getLines()
      .map { case s"Step $a must be finished before step $b can begin."
        => Step(a(0), b(0))
      }
      .toList

  /*
    A letter is considered free if it is only present in the set on the left side.
  */
  def freeLetters(steps: List[Step]): Set[Char] =
    val leftSet = steps.map(_.left).toSet
    val rightSet = steps.map(_.right).toSet
    leftSet diff rightSet

  /*
    The last letter of the sequence will be the letter that is only
    present in the set on the right side.
   */
  def getLastLetter(steps: List[Step]): Char =
    val lettersLeftSet = steps.map(_.left).distinct.sorted.toSet
    val lettersRightSet = steps.map(_.right).distinct.sorted.toSet
    lettersRightSet.filterNot(lettersLeftSet).max

  /*
    In the loop (the else condition)
      1. Determine the next letter of the sequence and add to the accumulator
      2. Remove all steps where this letter is on the left side
          Example: A is the next letter and steps A -> B and A -> D are present
          in the list of steps and there are no other steps to get to D.
          Here we remove both A -> B and A -> D and D is now also a 'free' letter
  */
  def getOrder(steps: List[Step]): Queue[Char] =
    val lastLetter = getLastLetter(steps)
    def loop(stepsAvailable: List[Step], acc: Queue[Char]): Queue[Char] =
      if (stepsAvailable.isEmpty)
        acc :+ lastLetter
      else
        val nextLetter = freeLetters(stepsAvailable).min
        val stepsToRemove = stepsAvailable.filter(_.left == nextLetter)
        loop(stepsAvailable diff stepsToRemove, acc :+ nextLetter)
    loop(steps, Queue.empty)

  val answer1: String = getOrder(inputSteps).mkString
  println(Console.BLUE + s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /* Part 2 */
  val start2: Long =
    System.currentTimeMillis

  case class Worker(secLeft: Int, letter: Char, working: Boolean)

  def updateTimeWorkers(w: List[Worker]): List[Worker] =
    val updatedWorkers = w.map(worker => {
      if (worker.secLeft > 0)
        worker.copy(secLeft = worker.secLeft - 1)
      else
        worker
    })
    updatedWorkers

  def getLettersJustFinished(w: List[Worker]): Set[Char] =
    w.collect {
      case Worker(0, letter, true) => letter
    }.toSet

  def updateWorkersDone(w: List[Worker]): List[Worker] =
    val updatedWorkers = w.map(worker => {
      if (worker.secLeft == 0)
        worker.copy(letter = ' ', working = false)
      else
        worker
    })
    updatedWorkers

  def assignWorkers(w: List[Worker], toAssign: Set[Char]): List[Worker] =
    var updatedWorkers = w

    for (l <- toAssign) {
      val indexNotWorking = updatedWorkers.indexWhere(!_.working)
      if (indexNotWorking != -1) {
        val workerToUpdate = updatedWorkers(indexNotWorking)
        val updatedWorker = workerToUpdate.copy(secLeft = l.toInt - 4, letter = l, working = true)
        updatedWorkers = updatedWorkers.updated(indexNotWorking, updatedWorker)
      }
    }
    updatedWorkers

  def collectWorkingLetters(w: List[Worker]): Set[Char] =
    w.collect {
      case Worker(_, letter, true) => letter
    }.toSet

  def updateSteps(steps: List[Step], lettersDone: Set[Char]): List[Step] =
    val stepsToRemove = steps.filter(step => lettersDone.contains(step.left))
    steps diff stepsToRemove

  def freeLetters2(w: List[Worker], steps: List[Step]): Set[Char] =
    val leftSet = steps.map(_.left).toSet
    val rightSet = steps.map(_.right).toSet
    leftSet diff rightSet diff collectWorkingLetters(w)

  def ans2(): Int = {
    var allSteps: List[Step] = inputSteps
    var workers: List[Worker] = List.fill(5)(Worker(0, ' ', false))
    var done = false
    var time = 0;
    var s = ""
    val lastLetter = getLastLetter(inputSteps)

    while (!done) {
      val justFinished = getLettersJustFinished(workers)
      if (justFinished.nonEmpty)
        allSteps = updateSteps(allSteps, justFinished)
        s += justFinished.mkString
      workers = updateWorkersDone(workers)
      workers = assignWorkers(workers, freeLetters2(workers, allSteps))
      workers = updateTimeWorkers(workers)

      if (allSteps.isEmpty && s.length == 25) {
        time += lastLetter.toInt - 4
        done = true
      }
      else
        time += 1
    }
    time
  }

  val answer2: Int = ans2()

  println(Console.BLUE + s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
