case class Resources(ore: Int, clay: Int, obs: Int, geode: Int):
  def +(other: Resources): Resources = 
    Resources(ore + other.ore, clay + other.clay, obs + other.obs, geode + other.geode)
  def -(other: Resources): Resources = 
    Resources(ore - other.ore, clay - other.clay, obs - other.obs, geode - other.geode)
  def <=(other: Resources): Boolean = 
    ore <= other.ore && clay <= other.clay && obs <= other.obs && geode <= other.geode
  
val zeroBot = Resources(0, 0, 0, 0)
val oreBot = Resources(1, 0, 0, 0)
val clayBot = Resources(0, 1, 0, 0)
val obsBot = Resources(0, 0, 1, 0)
val geodeBot = Resources(0, 0, 0, 1)

extension (s: Seq[String])
  def solve1: Int = s.maximise(24).sum

  def solve2: Int = s.take(3).maximise(32).product / 6

  private def maximise(time: Int): Seq[Int] = s.map { line => 
    val Seq(id, o1, o2, o3, clay, o4, obs) = line.split("\\D+").tail.map(_.toInt).toSeq
    val oreBotCost = Resources(o1, 0, 0, 0)
    val clayBotCost = Resources(o2, 0, 0, 0)
    val obsBotCost = Resources(o3, clay, 0, 0)
    val geodeBotCost = Resources(o4, 0, obs, 0)
    val maxOre = o1.max(o2).max(o3).max(o4)

    def aux(t: Int, bs: Resources, rs: Resources, prevOre: Boolean, prevClay: Boolean, prevObs: Boolean): Int =
      if t == 0 then
        rs.geode
      else if geodeBotCost <= rs then
        aux(t - 1, bs + geodeBot, rs + bs - geodeBotCost, false, false, false)
      else
        val canOre = oreBotCost <= rs && bs.ore < maxOre
        val canClay = clayBotCost <= rs && bs.clay < clay
        val canObsidian = obsBotCost <= rs && bs.obs < obs
        val first = aux(t - 1, bs, rs + bs, canOre, canClay, canObsidian)
        val second = if canOre && !prevOre then aux(t - 1, bs + oreBot, rs + bs - oreBotCost, false, false, false) else 0
        val third = if canClay && !prevClay then aux(t - 1, bs + clayBot, rs + bs - clayBotCost, false, false, false) else 0
        val fourth = if canObsidian && !prevObs then aux(t - 1, bs + obsBot, rs + bs - obsBotCost, false, false, false) else 0
        first.max(second).max(third).max(fourth)
  
    id * aux(time, oreBot, zeroBot, false, false, false)
  }

@main 
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)