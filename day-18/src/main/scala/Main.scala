type Cube = (Int, Int, Int)

val units = Seq((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))

extension (cube: Cube)
  def +(other: Cube): Cube = (cube._1 + other._1, cube._2 + other._2, cube._3 + other._3)
  def neighbours: Seq[Cube] = units.map(_ + cube)

extension (s: Seq[String])
  def solve1: Int = 
    val cubes = parse
    cubes.toSeq.map(_.neighbours.filterNot(cubes.contains).size).sum

  def solve2: Int = 
    val cubes = parse
    val xs = cubes.map(_._1).min - 1 to cubes.map(_._1).max + 1
    val ys = cubes.map(_._2).min - 1 to cubes.map(_._2).max + 1
    val zs = cubes.map(_._3).min - 1 to cubes.map(_._3).max + 1
    val start = (xs.head, ys.head, zs.head)
    val q = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)

    while q.nonEmpty do
      q.dequeue().neighbours.filterNot(cubes.contains).filterNot(visited.contains).foreach { next =>
        if xs.contains(next._1) && ys.contains(next._2) && zs.contains(next._3) then
          q.enqueue(next)
          visited += next
      }
    cubes.toSeq.map(_.neighbours.count(visited.contains)).sum

  private def parse: Set[Cube] = s
    .toSet
    .map(_.split(",").map(_.toInt))
    .map { case Array(x, y, z) => (x, y, z) }

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)