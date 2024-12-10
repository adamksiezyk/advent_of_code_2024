import scala.annotation.tailrec

@main def day3(): Unit = {
  val input = io.Source.fromFile("input.txt").getLines
//   val input = """190: 10 19
// 3267: 81 40 27
// 83: 17 5
// 156: 15 6
// 7290: 6 8 6 15
// 161011: 16 10 13
// 192: 17 8 14
// 21037: 9 7 18 13
// 292: 11 6 16 20""".linesIterator
  println(solution2(input))
}

def solution1(input: Iterator[String]): Long = {
  input
    .map(_.split(": ", 2))
    .map(x => (x(0).toLong, x(1).split(" ").map(_.toLong).toList))
    // .map((testValue, numbers) => (testValue, reduce(numbers)))
    .flatMap((testValue, numbers) =>
      if (reduce1(numbers).filter(_ == testValue).size == 0) None
      else Some(testValue)
    )
    .toList
    // .foreach(println)
    .sum
}

def solution2(input: Iterator[String]): Long = {
  input
    .map(_.split(": ", 2))
    .map(x => (x(0).toLong, x(1).split(" ").map(_.toLong).toList))
    // .map((testValue, numbers) => (testValue, reduce(numbers)))
    .flatMap((testValue, numbers) =>
      if (reduce2(numbers).filter(_ == testValue).size == 0) None
      else Some(testValue)
    )
    .toList
    // .foreach(println)
    .sum
}

def reduce1(numbers: List[Long]): List[Long] = {
  if (numbers.length == 2)
    List(numbers(0) * numbers(1), numbers(0) + numbers(1))
  else {
    val init = reduce1(numbers.init)
    val last = numbers.last
    (init.map(_ * last) ++ init.map(_ + last)).toList
  }
}

def reduce2(numbers: List[Long]): List[Long] = {
  if (numbers.length == 2)
    List(
      numbers(0) * numbers(1),
      numbers(0) + numbers(1),
      concat(numbers(0), numbers(1))
    )
  else {
    val init = reduce2(numbers.init)
    val last = numbers.last
    (init.map(_ * last) ++ init.map(_ + last) ++ init.map(
      concat(_, last)
    )).toList
  }
}

def concat(a: Long, b: Long): Long = (a.toString + b.toString).toLong
