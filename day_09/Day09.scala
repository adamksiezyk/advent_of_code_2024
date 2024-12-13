def solution1() = {
  // val input = "2333133121414131402"
  val input = scala.io.Source.fromFile("input.txt").mkString.strip
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
  val checksum = diskCompressed.zipWithIndex
    .map(_ * _)
    .sum
  println(checksum)
}

def rearrange(entry: Vector[String]): Vector[Long] =
  Iterator
    .unfold((0, entry.size - 1)) { (idxStart, idxEnd) =>
      if (idxStart >= idxEnd)
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
