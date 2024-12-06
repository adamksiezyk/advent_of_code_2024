import java.util.HashMap
@main def hello(): Unit = {
  val source = scala.io.Source.fromFile("input.txt")
  val input1 = source.getLines()
  // println("Solution 1")
  // println(solve1(input1))
  println("Solution 2")
  println(solve2(input1))
  source.close()
}

def solve1(input: Iterator[String]): Int = {
  val arrays = input
    .map(line => line.split("   ").map(_.toInt))
    .map { line =>
      (line(0), line(1))
    }
    .toSeq
  val (arr1, arr2) = arrays.unzip
  arr1.sorted.zip(arr2.sorted).map((x1, x2) => Math.abs(x1 - x2)).sum
}

def solve2(input: Iterator[String]): Int = {
  val arrays = input
    .map(line => line.split("   ").map(_.toInt))
    .map { line =>
      (line(0), line(1))
    }
    .toSeq
  val (arr1, arr2) = arrays.unzip

  var countMap = new HashMap[Int, Int]()
  for (id <- arr2) {
    countMap.put(id, countMap.getOrDefault(id, 0) + 1)
  }

  var sum = 0
  for (id <- arr1) {
    sum += id * countMap.getOrDefault(id, 0)
  }
  sum
}
