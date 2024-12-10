import scala.annotation.tailrec

object Symbols {
  val field = '.'
  val wall = '#'
  val guard = List('^', 'v', '<', '>')
}

case class Guard(symbol: Char, x: Int, y: Int)

@main def day6(): Unit = {
//   val input = """....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#..."""
  val input = io.Source.fromFile("input.txt").mkString
  println(solution2(input))
}

def solution2(input: String): Int = {
  var guard = Guard.apply tupled input.linesIterator.zipWithIndex
    .map((line, y) => line.zipWithIndex.map((char, x) => (char, x, y)))
    .flatten
    .filter((char, _, _) => Symbols.guard.contains(char))
    .next
  var labMap = input.linesIterator.zipWithIndex
    .map((line, y) =>
      line.zipWithIndex.map((char, x) =>
        if (x == guard.x && y == guard.y) (x, y) -> Symbols.field
        else (x, y) -> char
      )
    )
    .flatten
    .toMap

  val (path, _) = evaluatePath(labMap, guard)
  val obstacles = path.zipWithIndex
    .flatMap(el => {
      val ((x, y), i) = el
      println(f"Processing: ${i + 1}/${path.size}")
      evaluatePath(labMap.updated((x, y), Symbols.wall), guard) match {
        case (_, true)  => Some(x, y)
        case (_, false) => None
      }
    })
    .toList

  obstacles.size
}

/** Evaluates the path of a guard.
  *
  * @param labMap
  *   Map of the game coords -> field
  * @param guardInit
  *   Initial guard
  * @return
  *   The evaluated path of the guard and an isLoop flag
  */
def evaluatePath(
    labMap: Map[(Int, Int), Char],
    guardInit: Guard
): (List[(Int, Int)], Boolean) = {

  val xMax = labMap.keys.map(_._1).max
  val yMax = labMap.keys.map(_._2).max

  @tailrec
  def inner(path: List[Guard], guard: Guard): (List[Guard], Boolean) = {
    val pathNew = path.appended(guard)
    val (xNew, yNew) = step(guard)
    labMap.get((xNew, yNew)) match {
      case Some(symbol) => {
        val guardNew = symbol match {
          case Symbols.wall => Guard(turn(guard), guard.x, guard.y)
          case _            => Guard(guard.symbol, xNew, yNew)
        }
        guardNew match {
          case guardNew if path.contains(guardNew) => (pathNew, true)
          case guardNew                            => inner(pathNew, guardNew)
        }
      }
      case None => (pathNew, false)
    }
  }

  val (path, isLoop) = inner(List(), guardInit)
  val pathNew = path
    .map(guard => (guard.x, guard.y))
    .distinct
    .toList

  (pathNew, isLoop)
}

def solution1(input: String): Int = {
  val xMax = input.linesIterator.next.size - 1
  val yMax = input.linesIterator.size - 1
  var guard = Guard.apply tupled input.linesIterator.zipWithIndex
    .map((line, y) => line.zipWithIndex.map((char, x) => (char, x, y)))
    .flatten
    .filter((char, _, _) => Symbols.guard.contains(char))
    .next
  val labMap = input.linesIterator.zipWithIndex
    .map((line, y) =>
      line.zipWithIndex.map((char, x) =>
        if (x == guard.x && y == guard.y) (x, y) -> Symbols.field
        else (x, y) -> char
      )
    )
    .flatten
    .toMap

  val path = Iterator
    .unfold(guard) { guard =>
      val next = step(guard)
      labMap.get(next) match {
        case Some(Symbols.wall) =>
          Some((guard.x, guard.y), Guard(turn(guard), guard.x, guard.y))
        case _ => Some(next, Guard(guard.symbol, next._1, next._2))
      }
    }
    .takeWhile((x, y) => x >= 0 && x <= xMax && y >= 0 && y <= yMax)
    .toList
    .prepended((guard.x, guard.y))
    .distinct

  println("Guard start")
  println(guard)
  println("Guard path")
  plot(labMap, path)

  path.size
}

def step(guard: Guard): (Int, Int) = guard.symbol match {
  case '^' => guard.x -> (guard.y - 1)
  case 'v' => guard.x -> (guard.y + 1)
  case '<' => (guard.x - 1) -> guard.y
  case '>' => (guard.x + 1) -> guard.y
}

def turn(guard: Guard): Char = guard.symbol match {
  case '^' => '>'
  case '>' => 'v'
  case 'v' => '<'
  case '<' => '^'
}

def plot(labMap: Map[(Int, Int), Char], path: List[(Int, Int)]): Unit = {
  val pathMap = path.map((x, y) => (x, y) -> 'X').toMap
  val newLabMap = labMap ++ pathMap

  val maxX = newLabMap.keys.map(_._1).max
  val maxY = newLabMap.keys.map(_._2).max
  for (y <- 0 to maxY) {
    for (x <- 0 to maxX) {
      print(newLabMap((x, y)))
    }
    println()
  }
}
