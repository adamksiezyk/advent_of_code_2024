// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  val input = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

  test("Test Part 1") {
    val obtained = solution1(input.linesIterator)
    val expected: Long = 3749
    assertEquals(obtained, expected)
  }

  test("Test Part 2") {
    val obtained = solution2(input.linesIterator)
    val expected: Long = 11387
    assertEquals(obtained, expected)
  }
}
