type Point = (Int, Int)
type RockShape = Set[Point]

val rocks: Seq[RockShape] = Seq(
    Set((0, 0), (1, 0), (2, 0), (3, 0)),
    Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
    Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)),
    Set((0, 0), (0, 1), (0, 2), (0, 3)),
    Set((0, 0), (1, 0), (0, 1), (1, 1))
  )

extension (shape: RockShape)
  def move(dx: Int, dy: Int): RockShape = shape.map { case (x, y) => (x + dx, y + dy) }
  def canMove(cave: Set[Point]): Boolean = shape.forall((x, y) => x > 0 && x < 8 && !cave.contains((x, y)))

case class State(jets: String, grid: Set[Point], shapeIndex: Int, jetIndex: Int, height: Int):
  def step: State =
    val initialShape = rocks(shapeIndex % rocks.size).move(3, height + 4)
    val (nextShape, nextJetIndex) = fall(initialShape, jetIndex)
    val nextHeight = height.max(nextShape.map(_._2).max)
    State(jets, grid ++ nextShape, shapeIndex + 1, nextJetIndex, nextHeight)

  def fall(shape: Set[Point], jetIndex: Int): (Set[Point], Int) =
    val jet = jets(jetIndex % jets.length)
    val first = if jet == '>' then shape.move(1, 0) else shape.move(-1, 0)
    val second = if first.canMove(grid) then first else shape
    val third = second.move(0, -1)
    if third.canMove(grid) then fall(third, jetIndex + 1) else (second, jetIndex + 1)

extension (s: String) 
  def solve1: Int = simulate.drop(2022).next()

  def solve2: Long = 
    val guess = 1000
    val height = simulate.slice(1, 5 * guess).toSeq
    val delta = height.sliding(2).map(s => s.last - s.head).toSeq
    val end = delta.size - guess
    val start = delta.lastIndexOfSlice(delta.takeRight(guess), end - 1)
    val cycleHeight = height(end) - height(start)
    val cycleWidth = end - start
    val offset = 1000000000000L - 1 - start
    val quotient = offset / cycleWidth
    val remainder = offset % cycleWidth
    (quotient * cycleHeight) + height(start + remainder.toInt)

  def simulate: Iterator[Int] =
    val initial = State(s, Set.tabulate(8)((_, 0)), 0, 0, 0)
    Iterator.iterate(initial)(_.step).map(_.height)

@main 
def hello: Unit = 
  val input = io.Source.fromFile("input.txt").getLines().toSeq.head

  println(input.solve1)
  println(input.solve2)