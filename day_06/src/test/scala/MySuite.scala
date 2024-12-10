// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  val input = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

  test("Test Part 1") {
    val obtained = solution1(input)
    val expected = 41
    assertEquals(obtained, expected)
  }

  test("Test no loop") {
    val inputMap = input.linesIterator.zipWithIndex
      .map((line, y) => line.zipWithIndex.map((char, x) => (x, y) -> char))
      .flatten
      .toMap
    val (_, obtained) = evaluatePath(inputMap, Guard('^', 4, 6))
    val expected = false
    assertEquals(obtained, expected)
  }

  test("Test loop #1") {
    val inputMap = """....#.....
.........#
..........
..#.......
.......#..
..........
.#.#^.....
........#.
#.........
......#...""".linesIterator.zipWithIndex
      .map((line, y) => line.zipWithIndex.map((char, x) => (x, y) -> char))
      .flatten
      .toMap
    val (_, obtained) = evaluatePath(inputMap, Guard('^', 4, 6))
    val expected = true
    assertEquals(obtained, expected)
  }

  test("Test loop #2") {
    val inputMap = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
......#.#.
#.........
......#...""".linesIterator.zipWithIndex
      .map((line, y) => line.zipWithIndex.map((char, x) => (x, y) -> char))
      .flatten
      .toMap
    val (_, obtained) = evaluatePath(inputMap, Guard('^', 4, 6))
    val expected = true
    assertEquals(obtained, expected)
  }

  test("Test Part 2") {
    val obtained = solution2(input)
    val expected = 6
    assertEquals(obtained, expected)
  }
}
