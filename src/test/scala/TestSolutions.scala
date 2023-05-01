import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 Part 1") {
    assertResult(547)(actual = Day01.answer1)
  }
  test("Day01 Part 2") {
    assertResult(76414)(actual = Day01.answer2)
  }
  test("Day02 Part 1") {
    assertResult(5928)(actual = Day02.answer1)
  }
  test("Day02 Part 2") {
    assertResult("bqlporuexkwzyabnmgjqctvfs")(actual = Day02.answer2)
  }
  test("Day03 Part 1") {
    assertResult(124850)(actual = Day03.answer1)
  }
  test("Day03 Part 2") {
    assertResult(1097)(actual = Day03.answer2)
  }
  test("Day04 Part 1") {
    assertResult(30630)(actual = Day04.answer1)
  }
  test("Day04 Part 2") {
    assertResult(999)(actual = Day04.answer2)
  }