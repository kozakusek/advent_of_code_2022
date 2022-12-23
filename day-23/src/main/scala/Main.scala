type Point = (Int, Int)
type State = (Set[Point], Seq[Point], Boolean)

val neighbours = Seq(
  (-1, -1), (0, -1), (1, -1), (-1, 0), 
  (1, 0), (-1, 1), (0, 1), (1, 1)
)
val north = (0, -1)
val south = (0, 1)
val west = (-1, 0)
val east = (1, 0)

extension (p: Point) def +(other: Point): Point = (p._1 + other._1, p._2 + other._2)
  
extension (s: Seq[String])
  def solve1: Int = 
    val start = (s.parse, Seq(north, south, west, east), false)
    val elves = Iterator.iterate(start)(step).drop(10).next._1
    val (minX, maxX) = (elves.map(_._1).min, elves.map(_._1).max)
    val (minY, maxY) = (elves.map(_._2).min, elves.map(_._2).max)
    val grid = for y <- minY to maxY; x <- minX to maxX yield (x, y)
    grid.filterNot(elves.contains).size

  def solve2: Int = 
    Iterator.iterate((s.parse, Seq(north, south, west, east), false))(step).indexWhere(_._3)

  private def parse: Set[Point] =
    (for y <- s.indices; x <- s.head.indices if s(y)(x) != '.' yield (x, y)).toSet
  
  private def proposal(elves: Set[Point], moves: Seq[Point], elf: Point): Option[Point] =
    val checks = neighbours.map(_ + elf).map(elves.contains)
    val Seq(nw, n, ne, w, e, sw, s, se) = checks
    if checks.exists(identity) then
      moves.find {
        case `north` => !(nw || n || ne)
        case `south` => !(sw || s || se)
        case `west` => !(nw || w || sw)
        case `east` => !(ne || e || se)
      }
      .map(_ + elf)
    else None
  
  private def step(state: State): State =
    val (elves, moves, _) = state
    val proposals = elves.map(elf => elf -> proposal(elves, moves, elf))
    val occurrences = proposals.toSeq.flatMap(_._2).groupMapReduce(identity)(_ => 1)(_ + _)
    val valid = occurrences.filter((_, total) => total == 1).keySet
    val next = proposals.map { (elf, proposal) =>
      proposal.map(move => if valid.contains(move) then move else elf).getOrElse(elf)
    }
    (next, moves.tail :+ moves.head, elves == next)

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)