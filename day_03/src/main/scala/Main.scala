val MUL_PATTERN = """mul\((\d+),(\d+)\)""".r
val DO_PATTERN = """do\(\)""".r
val DONT_PATTERN = """don't\(\)""".r
val ALL_OPERATIONS_PATTERN = f"""$MUL_PATTERN|$DO_PATTERN|$DONT_PATTERN""".r

@main def day3(): Unit = {
  val input = io.Source.fromFile("input.txt").getLines
  // val input =
  //   "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))".linesIterator
  println(solution2(input))
}

def solution1(input: Iterator[String]): Int = input
  .map(MUL_PATTERN.findAllMatchIn)
  .flatten
  .map(m => m.group(1).toInt * m.group(2).toInt)
  .sum

def solution2(input: Iterator[String]): Int = {
  var doMul = true
  input
    .map(ALL_OPERATIONS_PATTERN.findAllMatchIn)
    .flatten
    .flatMap(_ match {
      case DO_PATTERN() => {
        doMul = true
        None
      }
      case DONT_PATTERN() => {
        doMul = false
        None
      }
      case MUL_PATTERN(num1, num2) if (doMul) => {
        Some(num1.toInt * num2.toInt)
      }
      case _ => None
    })
    .sum
}
