// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Test Part 1") {
    val input =
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))".linesIterator
    val obtained = solution1(input)
    val expected = 161
    assertEquals(obtained, expected)
  }

  test("Test Part 2") {
    val input =
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))".linesIterator
    val obtained = solution2(input)
    val expected = 48
    assertEquals(obtained, expected)
  }
}
