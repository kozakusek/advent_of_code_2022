extension (p: (Long, Long))
  def distressFreq: Long = p._1 * 4000000 + p._2

extension (s: Seq[String])
  private def getBeacons: Set[(Long, Long)] = s
    .map {
      case s"$_ beacon is at x=$x, y=$y" => (x.toLong, y.toLong)
    }.toSet
  
  private def getSensors: Set[(Long, Long)] = s
    .map {
      case s"Sensor at x=$x, y=$y: $_" => (x.toLong, y.toLong)
    }.toSet

  private def getTakenRanges(limit: Long): List[(Long, Long)] =
    val (a, b, acc) = s
      .map {
        case s"Sensor at x=$sX, y=$sY: closest beacon is at x=$bX, y=$bY" => 
          ((sX.toLong, sY.toLong), (sX.toLong - bX.toLong).abs + (sY.toLong - bY.toLong).abs)
      }.filter { 
        case ((x, y), d) => (y <= limit && y + d >= limit) || (y >= limit && y - d <= limit)
      }.map {
        case ((x, y), d) => 
          val r =  d - (y - limit).abs
          (x - r, x + r)
      }.sorted
      .foldLeft((Long.MinValue, Long.MinValue, List.empty[(Long, Long)])) {
        case ((a, b, acc), (left, right)) =>
          if (left > b) (left, right, (a, b) :: acc)
          else (a, right.max(b), acc)
      }
    ((a, b) :: acc).filter {
      case (a, b) => a != Long.MinValue && b != Long.MinValue
    }

  def solve1(limit: Long): Long = 
    val beacons = s.getBeacons.filter(_._2 == limit)
    val taken = s.getTakenRanges(limit)
    val nOfBeacons = taken.foldLeft(0) {
      case (acc, (a, b)) => acc + beacons.count { case (x, y) => x >= a && x <= b }
    }
    taken.map { case (a, b) => b - a + 1 }.sum - nOfBeacons
  
  def solve2(limit: Long): Long =
    val beacons = s.getBeacons
    val sensors = s.getSensors
    var (x, y) = (0L, -1L)
    var found = false 
    while y <= limit && !found do 
      y += 1
      var taken = s.getTakenRanges(y).filter { 
        case (a, b) => (0 <= a && a <= limit) || (0 <= b && b <= limit) 
      }.sorted
      x = 0
      while x <= limit && taken.size > 0 && !found do
        val (a, b) = taken.head
        if x < a && !sensors.contains((x, y)) then found = true
        if !found then x = b + 1
        taken = taken.tail
    (x, y).distressFreq

val TEST = false
@main 
def Main: Unit = 
  val lines = io.Source.fromFile(if TEST then "test.txt" else "input.txt").getLines.toSeq
  
  println(lines.solve1(if TEST then 10 else 2000000))
  println(lines.solve2(if TEST then 20 else 4000000))
