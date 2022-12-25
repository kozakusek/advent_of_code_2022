extension (s: Seq[String])
  def solve: String = toSNAFU(s.map(fromSNAFU).sum)

  def fromSNAFU(snafu: String): Long = snafu.foldLeft(0L) { (acc, c) =>
    5 * acc + (c match {
      case '2' => 2
      case '1' => 1
      case '0' => 0
      case '-' => -1
      case '=' => -2
    })
  }
  
  def toSNAFU(n: Long): String = if n == 0 then "" else {
    toSNAFU((n + 2) / 5 ) + (n % 5 match {
      case 2 => "2"
      case 1 => "1"
      case 0 => "0"
      case 4 => "-"
      case 3 => "="
    })
  }

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve)
