import scala.collection.mutable.SortedMap
import scala.math.Ordering.Reverse

@main def main() =
  val inputTest = "2333133121414131402"
  val input = scala.io.Source.fromFile("input.txt").mkString.strip

  println("Part 1")
  val expectedPart1Test = 1928L
  val outputPart1Test = solution1(inputTest)
  assert(
    outputPart1Test == expectedPart1Test,
    f"$outputPart1Test != $expectedPart1Test"
  )
  println("Test passed")
  println("Solution")
  println(solution1(input))

  println("Part 2")
  val expectedPart2Test = 2858L
  val outputPart2Test = solution2(inputTest)
  assert(
    outputPart2Test == expectedPart2Test,
    f"$outputPart2Test != $expectedPart2Test"
  )
  println("Test passed")
  println("Solution")
  println(solution2(input))

def solution2(input: String): Long =
  // Create a files map and a free space map
  var filesMap: SortedMap[Int, Block] =
    SortedMap.empty[Int, Block](Ordering[Int].reverse)
  var freeMap: SortedMap[Int, Int] = SortedMap()
  var idx = 0
  for (i <- Range(0, input.size)) {
    val blockSize = input(i).toString.toInt
    if (blockSize > 0)
      if (i % 2 == 0)
        val blockId = i / 2
        filesMap = filesMap + (idx -> Block(blockId, blockSize))
      else freeMap = freeMap + (idx -> blockSize)
      idx += blockSize
  }

  // Move the right-most file blocks to the left-most free position or leave as is
  var denseFilesMap = filesMap.clone
  for ((idx, block) <- filesMap) {
    val freeBlock =
      freeMap
        .filter((freeIdx, freeSize) =>
          (freeIdx < idx) && (freeSize >= block.size)
        )
        .headOption
    freeBlock match {
      case Some(freeIdx, freeSize) => {
        denseFilesMap -= idx
        denseFilesMap += freeIdx -> block
        freeMap -= freeIdx
        freeMap += (freeIdx + block.size) -> (freeSize - block.size)
      }
      case None =>
    }
  }

  denseFilesMap
    .map((idx, block) => (idx until idx + block.size).sum * block.id.toLong)
    .sum

case class Block(id: Int, size: Int)

def plot(filesMap: SortedMap[Int, Block]) =
  val (lastIdx, lastBlock) = filesMap.head
  var outLine = List.fill(lastIdx + lastBlock.size)(".")
  for ((idx, block) <- filesMap) {
    for (i <- Range(idx, idx + block.size)) {
      outLine = outLine.updated(i, block.id.toString)
    }
  }
  println(outLine.mkString)

def solution1(input: String): Long = {
  val disk = input.iterator
    .map(_.toString.toInt)
    .zipWithIndex
    .map((digit, i) => {
      if (i % 2 == 0) Vector.fill(digit)((i / 2).toString)
      else Vector.fill(digit)(".")
    })
    .flatten
    .toVector
  val diskCompressed = rearrange(disk)
  diskCompressed.zipWithIndex
    .map(_ * _)
    .sum
}

def rearrange(entry: Vector[String]): Vector[Long] =
  Iterator
    .unfold((0, entry.size - 1)) { (idxStart, idxEnd) =>
      if (idxStart > idxEnd)
        None
      else
        (entry(idxStart), entry(idxEnd)) match {
          case (".", ".") => Some((None, (idxStart, idxEnd - 1)))
          case (".", block) =>
            Some((Some(block.toLong), (idxStart + 1, idxEnd - 1)))
          case (block, ".") =>
            Some((Some(block.toLong), (idxStart + 1, idxEnd - 1)))
          case (block, _) => Some((Some(block.toLong), (idxStart + 1, idxEnd)))
        }
    }
    .flatten
    .toVector

def rearrangeOld(entry: Vector[String]): Vector[String] =
  var idxStart = 0
  var idxEnd = entry.size - 1
  var output = entry
  while (idxStart < idxEnd) {
    if (entry(idxStart) == ".")
      if (entry(idxEnd).forall(_.isDigit))
        output = output.updated(idxStart, output(idxEnd)).updated(idxEnd, ".")
        idxStart += 1
        idxEnd -= 1
      else idxEnd -= 1
    else
      idxStart += 1
      if (entry(idxEnd) == ".")
        idxEnd -= 1
  }
  output
