val DEBUG = false

enum Task {
  case One, Two
}

extension (s: Seq[String])
  def solve(task: Task): String = task match
    case Task.One => s.solveOne.toString
    case Task.Two => s.solveTwo.mkString("\n")
  
  private def solveOne: Int =
    val specialCycles = (20 to 220 by 40).toSet
      s.foldLeft((1, 1, 0)) { case ((x, cycle, strengths), line) =>
        var strMod = if specialCycles(cycle) then cycle * x else 0
        line match {
          case "noop" => (x, cycle + 1, strengths + strMod)
          case s"addx $v" => 
            if specialCycles(cycle + 1) then strMod = (cycle + 1) * x
            (x + v.toInt, cycle + 2, strengths + strMod)
        }
      }._3
  
  private def solveTwo: Iterator[String] = 
    val (_, cycles, marked) = s
      .foldLeft((1, 0, Set.empty[Int])) { case ((x, cycle, marked), line) =>
        val marked_1 = if (cycle % 40).hasSprite(x) then marked + cycle else marked
        line match {
          case "noop" => (x, cycle + 1, marked_1)
          case s"addx $v" => 
            val marked_2 = if ((cycle + 1) % 40).hasSprite(x) then marked_1 + (cycle + 1) else marked_1
            (x + v.toInt, cycle + 2, marked_2)
        }
      }
    (0 until cycles).map { cycle =>
      if marked(cycle) then "#" else "."
    }.grouped(40).map(_.mkString)

extension (cycle: Int)
  def hasSprite(x: Int): Boolean = x - 1 <= cycle && cycle <= x + 1 

@main 
def Main: Unit = 
  val lines = 
    if DEBUG then 
      io.Source.fromFile("test.txt").getLines.toSeq
    else
      io.Source.fromFile("input.txt").getLines.toSeq
  
  println(lines.solve(Task.One))
  println(lines.solve(Task.Two))

