@main def day5(): Unit = {
//   val input = """47|53
// 97|13
// 97|61
// 97|47
// 75|29
// 61|13
// 75|53
// 29|13
// 97|29
// 53|29
// 61|53
// 97|53
// 61|29
// 47|13
// 75|47
// 97|75
// 47|61
// 75|61
// 47|29
// 75|13
// 53|13
//
// 75,47,61,53,29
// 97,61,53,29,13
// 75,29,13
// 75,97,47,61,53
// 61,13,29
// 97,13,75,29,47""".linesIterator
  val input = io.Source.fromFile("input.txt").getLines
  println(solution2(input))
}

def solution1(input: Iterator[String]): Int = {
  val mustBeBefore =
    input
      .takeWhile(_.nonEmpty)
      .map(_.split('|').map(_.toInt))
      .map(x => x(1) -> x(0))
      .toList
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toMap

  input
    .map(_.split(',').map(_.toInt).toVector)
    .map(arr =>
      (
        arr,
        shrinkingWindows(arr)
          .flatMap(window => {
            val page = window(0)
            val rest = window.drop(0)
            mustBeBefore.get(page) match {
              case Some(invalid) if (!invalid.intersect(rest).isEmpty) =>
                Some(page, invalid.intersect(rest))
              case _ => None
            }
          })
          .toVector
      )
    )
    .filter(_._2.isEmpty)
    .map((arr, _) => arr(arr.size / 2))
    .sum
}

def solution2(input: Iterator[String]): Int = {
  val mustBeBefore =
    input
      .takeWhile(_.nonEmpty)
      .map(_.split('|').map(_.toInt))
      .map(x => x(1) -> x(0))
      .toList
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toMap

  input
    .map(_.split(',').map(_.toInt).toVector)
    .map(arr =>
      (
        arr,
        shrinkingWindows(arr)
          .flatMap(window => {
            val page = window(0)
            val rest = window.drop(0)
            mustBeBefore.get(page) match {
              case Some(invalid) if (!invalid.intersect(rest).isEmpty) =>
                Some(page, invalid.intersect(rest))
              case _ => None
            }
          })
          .toVector
      )
    )
    .filter(_._2.nonEmpty)
    .map((arr, violations) => {
      arr
        .map(i =>
          (
            i,
            mustBeBefore.getOrElse(i, List()).filter(i => arr.contains(i))
          )
        )
    })
    .map(_.sortBy(_._2.size))
    .map(_.map(_._1).toVector)
    .map(arr => arr(arr.size / 2))
    .sum
}

def shrinkingWindows[A](vec: Vector[A]): List[Vector[A]] = {
  if (vec.isEmpty) List.empty
  else vec +: shrinkingWindows(vec.tail)
}
