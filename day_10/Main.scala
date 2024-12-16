val MAX_HEIGHT = 9

@main def main() =
  val inputTest = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""
  val input = scala.io.Source.fromFile("input.txt").mkString.strip

  println("Part 1")
  val expectedPart1Test = 36
  val outputPart1Test = solution1(inputTest)
  assert(
    outputPart1Test == expectedPart1Test,
    f"$outputPart1Test != $expectedPart1Test"
  )
  println("Test passed")
  println("Solution")
  println(solution1(input))

  println("Part 2")
  val expectedPart2Test = 81
  val outputPart2Test = solution2(inputTest)
  assert(
    outputPart2Test == expectedPart2Test,
    f"$outputPart2Test != $expectedPart2Test"
  )
  println("Test passed")
  println("Solution")
  println(solution2(input))

def solution2(input: String): Int =
  val grid = input.linesIterator.zipWithIndex
    .map((line, y) =>
      line.zipWithIndex.map((c, x) => (x, y) -> c.toString.toInt)
    )
    .flatten
    .toMap

  val trailheads = grid
    .filter(_._2 == 0)
    .keys
    .toList
  val paths = trailheads.map(p => step(grid, p._1, p._2, -1).size)
  paths.sum

def solution1(input: String): Int =
  val grid = input.linesIterator.zipWithIndex
    .map((line, y) =>
      line.zipWithIndex.map((c, x) => (x, y) -> c.toString.toInt)
    )
    .flatten
    .toMap

  val trailheads = grid
    .filter(_._2 == 0)
    .keys
    .toList
  val paths = trailheads.map(p => step(grid, p._1, p._2, -1).distinct.size)
  paths.sum

def step(
    grid: Map[(Int, Int), Int],
    x: Int,
    y: Int,
    prevHeight: Int
): List[(Int, Int)] =
  grid.get((x, y)) match {
    case Some(height) if (height - prevHeight == 1) => {
      if (height == MAX_HEIGHT) {
        List((x, y))
      } else {
        List(
          step(grid, x + 1, y, height),
          step(grid, x - 1, y, height),
          step(grid, x, y + 1, height),
          step(grid, x, y - 1, height)
        ).flatten
      }
    }
    case _ => List.empty
  }
