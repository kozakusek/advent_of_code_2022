enum Task {
  case One, Two
}

type Point = (Int, Int)

val DEBUG = false

def dirToDelta(dir: Char): Point = dir match {
  case 'U' => (0, 1)
  case 'D' => (0, -1)
  case 'L' => (-1, 0)
  case 'R' => (1, 0)
}

def debug(head: Point, tail : List[Point], visited: Set[Point]): Unit = 
  val maxX = visited.map(_._1).max.max(head._1).max(tail.map(_._1).max)
  val minX = visited.map(_._1).min.min(head._1).min(tail.map(_._1).min)
  val maxY = visited.map(_._2).max.max(head._2).max(tail.map(_._2).max)
  val minY = visited.map(_._2).min.min(head._2).min(tail.map(_._2).min)

  println(s"Head: $head, Tail: ${tail.mkString(", ")}")  

  for y <- (minY to maxY).reverse do
    for x <- minX to maxX do
      if x == head._1 && y == head._2 then print("H")
      else if tail.contains((x, y)) then 
        val pos = tail.indexOf((x, y))
        if pos == tail.size - 1 then print("T")
        else print(pos + 1)
      else if x == 0 && y == 0 then print("s")
      else if visited.contains((x, y)) then print("#")
      else print(".")
    println()
  
  println()

extension (s: Seq[String])
  def solve(t: Task): Int = 
    val start = (0, 0)
    val knots = t match {
      case Task.One => 1
      case Task.Two => 9
    }
    val starts = (1 to knots).map(_ => start).toList
    s.foldLeft((start, starts, Set(start))) {
      case (state, instr) =>
        val (dir, steps) = (instr.head, instr.tail.strip.toInt)
        val (h, t, v) = simulate(state, dirToDelta(dir), steps)
        if DEBUG then 
          println(s"Dir: $dir, Steps: $steps")
          debug(h, t, v)
        (h, t, v)
    }._3.size

def areAdjacent(a: Point, b: Point): Boolean = 
  val (ax, ay) = a
  val (bx, by) = b
  (ax - bx).abs <= 1  && (ay - by).abs <= 1

def getNewTailPos(head: Point, tail: Point): Point =
  if areAdjacent(head, tail) then return tail 
  val (hx, hy) = head
  val (tx, ty) = tail
  (if hx == tx then hx else if hx > tx then tx + 1 else tx - 1, 
   if hy == ty then hy else if hy > ty then ty + 1 else ty - 1)

def simulate(state: (Point, List[Point], Set[Point]), dir: Point, steps: Int): (Point, List[Point], Set[Point]) = 
  if steps == 0 then 
    return state

  val ((x, y), tail, visited) = state
  val (dx, dy) = dir
  val newHead = (x + dx, y + dy)
  var newTail = List.empty[Point]
  var prev = newHead
  for t <- tail do
    newTail = getNewTailPos(prev, t) :: newTail
    prev = newTail.head
    
  simulate((newHead, newTail.reverse, visited + newTail.head), dir, steps - 1)

@main
def Main: Unit = 
  val lines = 
    if DEBUG then
      //io.Source.fromFile("test.txt").getLines.toSeq
      io.Source.fromFile("bigTest.txt").getLines.toSeq
    else
      io.Source.fromFile("input.txt").getLines.toSeq
  
  println(lines.solve(Task.One))
  println(lines.solve(Task.Two))
