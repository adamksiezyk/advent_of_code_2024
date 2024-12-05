// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Test Part 1") {
    val input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".linesIterator
    val obtained = solution1(input)
    val expected = 2
    assertEquals(obtained, expected)
  }

  test("Test Part 2") {
    val input = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".linesIterator
    val obtained = solution2(input)
    val expected = 4
    assertEquals(obtained, expected)
  }
}
