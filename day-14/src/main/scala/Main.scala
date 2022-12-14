type Pos = (Int, Int)

val start: Pos = (500, 0)
val moves: Seq[Pos] = Seq((0, 1), (-1, 1), (1, 1))  

extension (p: Pos)
  def +(q: Pos): Pos = (p._1 + q._1, p._2 + q._2)

def fallingSand(obstacles: Set[Pos], end: Int, sand: Pos): Pos =
  if sand._2 == end then sand
  else moves.map(_ + sand).filterNot(obstacles.contains) match
    case Seq() => sand
    case s => fallingSand(obstacles, end, s.head)

def simulate(rocks: Set[Pos], end: Int, obstacles: Set[Pos], pred: Pos => Boolean) : Int =
  val sand = fallingSand(obstacles, end, start)
  if pred(sand) then obstacles.size - rocks.size else simulate(rocks, end, obstacles + sand, pred)

extension (s: Seq[String])

  def solve1: Int = 
    val (rocks, end) = s.parse
    simulate(rocks, end, rocks, _._2 == end)

  def solve2: Int = 
    val (rocks, end) = s.parse
    simulate(rocks, end, rocks, _ == start) + 1

  def parse: (Set[Pos], Int) = 
    val obstacles = s.toSet
      .flatMap(_
        .split("\\D+")
        .map(_.toInt)
        .sliding(4, 2)
        .flatMap {
          case Array(x1, y1, x2, y2) => 
            (x1.min(x2) to x1.max(x2))
            .flatMap(x => (y1.min(y2) to y1.max(y2))
              .map(y => (x, y)))
        }
      )
    (obstacles, obstacles.map(_._2).max + 1)


@main
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)
