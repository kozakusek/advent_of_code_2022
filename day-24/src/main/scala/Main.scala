type Point = (Int, Int)

extension (p: Point)
  def +(q: Point): Point = (p._1 + q._1, p._2 + q._2)
  def neighbours: Seq[Point] = 
    Seq(p, (p._1 + 1, p._2), (p._1 - 1, p._2), (p._1, p._2 + 1), (p._1, p._2 - 1))

extension (s: Seq[String])
  def solve1: Int =
    val (end, sim) = parse
    sim(Set((1, 0)), end, 0)

  def solve2: Int = 
    val (end, sim) = parse
    val s2et = sim(Set((1, 0)), end, 0)
    val e2st = sim(Set(end), (1, 0), s2et)
    sim(Set((1, 0)), end, e2st)

  private def parse: (Point, (Set[Point], Point, Int) => Int) =
    val (w, h) = (s.head.size - 2, s.size - 2)

    def div(n: Int, m: Int) = 
      val r = (n - 1) % m
      r + 1 + (if r < 0 then m else 0)
    
    def valid(t: Int)(p: Point): Boolean = 
      val (x, y) = p
      s.indices.contains(y) 
        && s(y)(x) != '#'
        && s(y)(div(x + t, w)) != '<'
        && s(y)(div(x - t, w)) != '>'
        && s(div(y + t, h))(x) != '^'
        && s(div(y - t, h))(x) != 'v'
    
    def simulate(points: Set[Point], end: Point, t: Int): Int = 
      if points.contains(end) then t else 
        simulate(points.flatMap(_.neighbours).filter(valid(t + 1)), end, t + 1)
    
    ((w, h + 1), simulate)
    

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)