// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Part 1") {
    val input = """3   4
4   3
2   5
1   3
3   9
3   3""".linesIterator
    val obtained = solve1(input)

    val expected = 11
    assertEquals(obtained, expected)
  }

  test("Part 2") {
    val input = """3   4
4   3
2   5
1   3
3   9
3   3""".linesIterator
    val obtained = solve2(input)

    val expected = 31
    assertEquals(obtained, expected)
  }
}
