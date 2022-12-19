type Valve = (Int, Seq[String])

extension (s: Seq[String])
  def solve1: Int = explore(30).values.max

  def solve2: Int =
    val sets = explore(26)
    (for { (me, meScore) <- sets; (el, elScore) <- sets; if me.intersect(el).size == 0 }
      yield meScore + elScore
    ).max

  private def parse: (Map[String, Valve], Map[String, Map[String, Int]], Set[String])  = 
    val valves = s
      .map { line =>
        val Array(_, name, flow, edges: _*) = line.split("[^A-Z0-9]+"): @unchecked
        name -> (flow.toInt, edges)
      }.toMap
    val distances = valves.map((k, v) => k -> bfs(valves, k))
    val goodValves = valves.filter((k, v) => v._1 > 0).keySet
    (valves, distances, goodValves)

  private def bfs(graph: Map[String, Valve], source: String):  Map[String, Int] =
    val q = collection.mutable.Queue(source)
    val cost = collection.mutable.Map(source -> 1)

    while q.nonEmpty do
      val current = q.dequeue()
      graph(current)._2.filterNot(cost.contains).foreach { next =>
        q.enqueue(next)
        cost(next) = cost(current) + 1
      }

    cost.toMap

  private def explore(initialTime: Int): Map[Set[String], Int] =
    val (valves, distance, todo) = s.parse
    val score = collection.mutable.Map[Set[String], Int]().withDefaultValue(0)

    def step(todo: Set[String], done: Set[String], from: String, time: Int, pressure: Int): Unit =
      score(done) = score(done).max(pressure)
      for
        next <- todo
        remaining = time - distance(from)(next)
        if remaining > 0
        extra = remaining * valves(next)._1
      do step(todo - next, done + next, next, remaining, pressure + extra)
    end step

    step(todo, Set(), "AA", initialTime, 0)
    score.toMap
   

@main
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)
