case class Point(x: Int, y: Int)

@main def solution2() = {
//   val input = """............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"""
  val input = scala.io.Source.fromFile("input.txt").mkString

  val antennas = input.linesIterator.zipWithIndex
    .map((line, y) => line.zipWithIndex.map((c, x) => c -> Point(x, y)))
    .flatten
    .filter(_._1.isLetterOrDigit)
    .toList
    .groupBy(_._1)
    .mapValues(_.map(_._2).toList)
    .toMap
  val xMax = input.linesIterator.next.size - 1
  val yMax = input.linesIterator.size - 1

  val antinodes = antennas.values
    .map(_.combinations(2))
    .flatten
    .map(pair => {
      val p1 = pair(0)
      val p2 = pair(1)
      val deltaX = p1.x - p2.x
      val deltaY = p1.y - p2.y

      val points1 = Iterator
        .from(1)
        .map { i => Point(p1.x + i * deltaX, p1.y + i * deltaY) }
        .takeWhile(p => p.x >= 0 && p.x <= xMax && p.y >= 0 && p.y <= yMax)
        .toList
      val points2 = Iterator
        .from(1)
        .map { i =>
          Point(p2.x - i * deltaX, p2.y - i * deltaY)
        }
        .takeWhile(p => p.x >= 0 && p.x <= xMax && p.y >= 0 && p.y <= yMax)
        .toList
      List(p1, p2) ++ points1 ++ points2
    })
    .flatten
    .toList
    .distinct

  plot(input, antinodes)
  println(antinodes.size)
}

def solution1() = {
//   val input = """............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"""
  val input = scala.io.Source.fromFile("input.txt").mkString

  val antennas = input.linesIterator.zipWithIndex
    .map((line, y) => line.zipWithIndex.map((c, x) => c -> Point(x, y)))
    .flatten
    .filter(_._1.isLetterOrDigit)
    .toList
    .groupBy(_._1)
    .mapValues(_.map(_._2).toList)
    .toMap
  val xMax = input.linesIterator.next.size - 1
  val yMax = input.linesIterator.size - 1

  val antinodes = antennas.values
    .map(_.combinations(2))
    .flatten
    .map(pair => {
      val p1 = pair(0)
      val p2 = pair(1)
      val deltaX = p1.x - p2.x
      val deltaY = p1.y - p2.y

      List(
        Point(p1.x + deltaX, p1.y + deltaY),
        Point(p2.x - deltaX, p2.y - deltaY)
      )
    })
    .flatten
    .filter(p => p.x >= 0 && p.x <= xMax && p.y >= 0 && p.y <= yMax)
    .toList
    .distinct

  plot(input, antinodes)
  println(antinodes.size)
}

def plot(gird: String, antinodes: List[Point]) = {
  val newGrid = gird.linesIterator.zipWithIndex
    .map((line, y) =>
      line.zipWithIndex
        .map((c, x) =>
          (x, y) match {
            case point if antinodes.contains(Point(x, y)) => '#'
            case _                                        => c
          }
        )
        .mkString
    )
    .mkString("\n")

  println(newGrid)
}
