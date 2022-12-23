import scala.io.Source

enum Part {
  case One
  case Two
}

extension (s: String) def print: Unit = println(s)

extension (r: (Int, Int))
  def intersect(other: (Int, Int)): (Int, Int) = 
    (r.head.max(other.head), r.last.min(other.last))
  
  def contains(el: Int): Boolean = r.head.toInt <= el && el <= r.last.toInt

  def containsAll(el: (Int, Int)): Boolean = contains(el.head) && contains(el.last)

  def nonEmpty: Boolean = r.head.toInt <= r.last.toInt

extension (s: Seq[String])
  def solve(p: Part): String = p match
    case Part.One => s.task1
    case Part.Two => s.task2
  
  private def prep : Seq[((Int, Int), (Int, Int))] = 
     s.map(_.split(",").map(_.split("-")))
      .map(x => ((x(0)(0).toInt, x(0)(1).toInt), (x(1)(0).toInt, x(1)(1).toInt)))

  private def task1: String = 
    s.prep
      .map((a, b) => a.containsAll(b) || b.containsAll(a))
      .count(identity)
      .toString

  private def task2: String =
    s.prep
      .map((a, b) => a.intersect(b).nonEmpty)
      .count(identity)
      .toString

@main 
def Main: Unit = 
  val lines = Source.fromFile("input.txt").getLines.toSeq

  lines.solve(Part.One).print
  lines.solve(Part.Two).print
