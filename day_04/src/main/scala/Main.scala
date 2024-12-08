import scala.util.matching.Regex

val XMAS_PATTERN = "(?=(XMAS|SAMX))".r
val XMAS_PATTERN_LEN = 4
val MAS_PATTERN = "(?=(MAS|SAM))".r
val MAS_PATTERN_LEN = 3

@main def day4(): Unit = {
//   val input = """MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX"""
  val input = io.Source.fromFile("input.txt").mkString
  println(solution2(input))
}

def solution1(input: String): Int = {
  val scanXmas = scanLine(XMAS_PATTERN, _)
  val isXmas = isMatch(XMAS_PATTERN, _)

  val lineLen = input.linesIterator.next.length
  val horizontal = input.linesIterator.zipWithIndex
    .map((line, i) => scanXmas(line).map(j => (i, j)))
    .flatten
    .toVector
  val vertical =
    Range(0, lineLen)
      .map(i => input.linesIterator.map(_.charAt(i)).mkString)
      .zipWithIndex
      .map((line, i) => scanXmas(line).map(j => (j, i)))
      .flatten

  val lines = input.linesIterator.toVector
  val maxDiagStart = lineLen - XMAS_PATTERN_LEN + 1
  val diagonalLToR = Range(0, maxDiagStart)
    .map(i => Range(0, maxDiagStart).map(j => (i, j)))
    .flatten
    .flatMap((i, j) =>
      if (
        isXmas(
          Range(0, XMAS_PATTERN_LEN)
            .map(k => lines(i + k).charAt(j + k))
            .mkString
        )
      ) Some(i, j)
      else None
    )
    .toVector
  val diagonalRToL = Range(XMAS_PATTERN_LEN - 1, lineLen)
    .map(i => Range(0, maxDiagStart).map(j => (i, j)))
    .flatten
    .flatMap((i, j) =>
      if (
        isXmas(
          Range(0, XMAS_PATTERN_LEN)
            .map(k => lines(i - k).charAt(j + k))
            .mkString
        )
      ) Some(i, j)
      else None
    )
    .toVector

  horizontal
    .++(vertical)
    .++(diagonalLToR)
    .++(diagonalRToL)
    .size
}

def solution2(input: String): Int = {
  val scanMas = scanLine(MAS_PATTERN, _)
  val isMas = isMatch(MAS_PATTERN, _)

  val lines = input.linesIterator.toVector
  val lineLen = lines.head.length
  val blockStartMax = lineLen - MAS_PATTERN_LEN + 1
  val cross = Range(0, blockStartMax)
    .map(i => Range(0, blockStartMax).map(j => (i, j)))
    .flatten
    .flatMap((i, j) =>
      if (
        isMas(
          Range(0, MAS_PATTERN_LEN)
            .map(k => lines(i + k).charAt(j + k))
            .mkString
        ) &&
        isMas(
          Range(0, MAS_PATTERN_LEN)
            .map(k => lines(i + MAS_PATTERN_LEN - 1 - k).charAt(j + k))
            .mkString
        )
      ) Some(i, j)
      else None
    )

  cross.size
}

def scanLine(pattern: Regex, line: String): Vector[Int] =
  pattern.findAllMatchIn(line).map(_.start).toVector

def isMatch(pattern: Regex, line: String): Boolean =
  pattern.findFirstIn(line).isDefined
