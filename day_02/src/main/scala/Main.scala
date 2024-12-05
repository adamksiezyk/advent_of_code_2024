val MIN_DIFF = 1
val MAX_DIFF = 3

@main def hello(): Unit = {
  val input = io.Source.fromFile("input.txt").getLines
//   val input = """7 6 4 2 1
// 1 2 7 8 9
// 9 7 6 2 1
// 1 3 2 4 5
// 8 6 4 4 1
// 1 3 6 7 9""".linesIterator
  println(solution2(input))
}

def solution1(input: Iterator[String]): Int = {
  input
    .map(
      _.split(" ")
        .map(_.toInt)
        .sliding(2)
        .map(pair => pair(0) - pair(1))
        .toVector
    )
    .filter(isValid)
    .size
}

def isValid(report: Vector[Int]): Boolean = {
  report.forall(d => d >= MIN_DIFF && d <= MAX_DIFF) |
    report.forall(d => d <= -MIN_DIFF && d >= -MAX_DIFF)
}

def solution2(input: Iterator[String]): Int = {
  input
    .map(
      _.split(" ")
        .map(_.toInt)
        .toVector
    )
    .filter(report =>
      report.indices.exists { i =>
        isValid(
          report
            .patch(i, Vector.empty, 1)
            .sliding(2)
            .map(pair => pair(0) - pair(1))
            .toVector
        )
      }
    )
    .size
}
