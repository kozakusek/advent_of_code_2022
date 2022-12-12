type V = (Int, Int)
type E = (V, V)

def heightDist(a: Char, b: Char): Int = 
  val x = a match { case 'S' => 'a' case 'E' => 'z' case _ => a }
  val y = b match { case 'S' => 'a' case 'E' => 'z' case _ => b }
  y - x

extension (in: Array[String])
  def solveOne: Int = solve('S')

  def solveTwo: Int = solve('a')
    
  private def solve(sourceChar: Char) : Int =
    val vertices = (0 until in.length).flatMap(y => (0 until in(0).length).map(x => (x, y))).toSet
    val sources = in.find(sourceChar)
    val sink = in.find('E').head
    var result = Int.MaxValue
    for source <- sources do
      var distances = vertices.map(v => (v, Int.MaxValue)).toMap.updated(source, 0)
      var queue = List(source)
      while queue.nonEmpty do
        val v = queue.head
        queue = queue.tail
        val neighbours = in.adjacent(v._1, v._2, 1)
        neighbours.foreach { n =>
          if distances(n) > distances(v) + 1 then
            distances = distances.updated(n, distances(v) + 1)
            queue = queue :+ n
        }

      if distances(sink) < result then result = distances(sink)
    result


  private def find(v: Char) : List[V] = 
    in.zipWithIndex.foldLeft(List.empty[V]) { case (acc, (line, y)) =>
      line.indexOf(v) match
        case -1 => acc
        case x => (x, y) :: acc
    }

  private def adjacent(x: Int, y: Int, dist: Int): List[V] = 
    List((x, y-1), (x-1, y), (x+1, y), (x, y+1))
      .filter { case (a, b) => a >= 0 && b >= 0 && a < in(0).length && b < in.length }
      .filter { case (a, b) => heightDist(in(y)(x), in(b)(a)) <= dist }

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toArray

  println(lines.solveOne)
  println(lines.solveTwo)