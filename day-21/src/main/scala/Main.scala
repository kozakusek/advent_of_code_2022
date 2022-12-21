extension (s: Seq[String])
  def solve1: Long = parse("root")()

  def solve2: Long = 
    val monkeys = parse
    var start = 3_000_000_000_000L
    var mid = 0L
    var end = 4_000_000_000_000L

    while (start < end) do {
      mid = (start + end) / 2

      monkeys("humn") = () => mid
      val result = monkeys("pgnv")()
      val target = monkeys("wcnp")()

      if result < target then end = mid - 1
      else if result > target then start = mid + 1
      else start = end
    }

    mid

  private def parse: collection.mutable.Map[String, () => Long] =
    var monkeys = collection.mutable.Map[String, () => Long]()
    def compute(name: String): Long = monkeys(name)()
    s.foreach { line =>
      val Array(name, rest: _*) = line.split("[: ]+"): @unchecked
      monkeys(name) = rest match
        case Seq(number) => () => number.toLong
        case Seq(left, op, right) => op match
          case "+" => () => compute(left) + compute(right)
          case "-" => () => compute(left) - compute(right)
          case "*" => () => compute(left) * compute(right)
          case "/" => () => compute(left) / compute(right)
    }
    monkeys

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)