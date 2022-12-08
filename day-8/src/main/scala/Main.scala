import scala.util.control.Breaks._

enum Task {
  case One, Two
}

extension (ox: Seq[String])
  def solve(task: Task) : Int =
    task match
      case Task.One => ox.allVisible
      case Task.Two => ox.maxScore
  
  def allVisible : Int =
    val oxWithPos = ox.zipWithIndex.map(
      (x, i) => x.zipWithIndex.map((y, j) => (y.toInt, i, j)))
    val oyWithPos = oxWithPos.transpose
    val a = oxWithPos.map(_.countVisible).acc
    val b = oyWithPos.map(_.countVisible).acc
    val c = oxWithPos.map(_.reverse).map(_.countVisible).acc
    val d = oyWithPos.map(_.reverse).map(_.countVisible).acc
    a.union(b).union(c).union(d).size
  
  def maxScore : Int =
    val arr = ox.map(_.map(_.toInt - '0').toArray).toArray    
    var score = 0
    for i <- 0 until arr.length do
      for j <- 0 until arr(i).length do
        score = score.max(arr.getScore(i, j))
    score

extension (m: Array[Array[Int]])
  def getScore(x: Int, y: Int) : Int =
    val height = m(x)(y)
    var left = 0
    breakable {
      for i <- x - 1 to 0 by -1 do
        left = left + 1
        if m(i)(y) >= height then
          break
    }
    var right = 0
    breakable {
      for i <- x + 1 until m.length do
        right = right + 1
        if m(i)(y) >= height then
          break
    }
    var up = 0
    breakable {
      for i <- y - 1 to 0 by -1 do
        up = up + 1
        if m(x)(i) >= height then
          break
    }
    var down = 0
    breakable {
      for i <- y + 1 until m(x).length do
        down = down + 1
        if m(x)(i) >= height then
          break
    }
    left * right * up * down


extension (s: Seq[Set[(Int, Int)]])
  def acc = s.foldLeft(Set.empty[(Int, Int)]) { (acc, x) => acc ++ x }

extension(s: Seq[(Int, Int, Int)])
  def countVisible: Set[(Int, Int)] = 
    s.foldLeft((-1, Set.empty[(Int, Int)])) {
      case ((max, acc), curr) => 
        if curr._1 > max then (curr._1, acc + ((curr._2, curr._3)))
        else (max, acc)
    }._2

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve(Task.One))
  println(lines.solve(Task.Two))
