import scala.collection.mutable.Queue

val DEBUG = false

class Monkey(
    var items: Queue[Long],
    val operation: Long => Long, 
    val next: Long => Int, 
    var inspections: Long = 0):
  def mkString: String = 
    "M([" + items.mkString(",") + "], [" + 
      items.map(operation).mkString(",") + "], [" + 
      items.map(next).mkString(",") + "], " + inspections + ")"
  
  def inspect(manageWorries: Long => Long): Unit = 
    inspections += items.length
    items = items.map(operation)
    items = items.map(manageWorries)
  
  def hasItems: Boolean = items.nonEmpty

  def throwOne: (Long, Int) =
    val item = items.dequeue
    val monkey = next(item)
    (item, monkey)
  
  def catchOne(item: Long): Unit = items.enqueue(item)

extension (m: Array[Monkey])
  def monkeyBusiness: String = m
    .map(_.inspections)
    .sorted
    .takeRight(2)
    .reduce(_ * _)
    .toString

extension (s: String) 
  def toLongDef(default: Long): Long = 
    try s.toLong catch case _: NumberFormatException => default

extension (s: Seq[String])
  def solveOne: String  = 
    var monkeys = s.makeMonkeys
    simulate(monkeys, 20, _/3)
    monkeys.monkeyBusiness

  def solveTwo: String =
    var monkeys = s.makeMonkeys
    val mods = s.filter(_.trim.startsWith("Test")).map(_.split(" ").last.toLong)
    val mod = mods.reduce(_ * _) << 8
    simulate(monkeys, 10_000, _%mod)
    monkeys.monkeyBusiness

  private def makeMonkeys = s.grouped(7).toSeq.map(_.tail.makeMonkey).toArray

  private def simulate(monkeys: Array[Monkey], rounds: Int, manageWorries: Long => Long): Unit = 
    for round <- 1 to rounds do
      for i <- 0 until monkeys.length do
        monkeys(i).inspect(manageWorries)
        while monkeys(i).hasItems do
          val (item, next) = monkeys(i).throwOne
          monkeys(next).catchOne(item)

  private def makeMonkey: Monkey = 
    val items = s(0).trim match { 
      case s"Starting items: $items" => Queue.from(items.split(",").map(_.trim.toLong))
    }
    val op = s(1).trim match {
      case s"Operation: new = $op" => op match {
        case s"$a + $b" => (x: Long) => a.toLongDef(x) + b.toLongDef(x)
        case s"$a - $b" => (x: Long) => a.toLongDef(x) - b.toLongDef(x)
        case s"$a * $b" => (x: Long) => a.toLongDef(x) * b.toLongDef(x)
        case s"$a / $b" => (x: Long) => a.toLongDef(x) / b.toLongDef(x)  
      }
    }
    val optionTrue = s(3).trim.split(" ").last.toInt
    val optionFalse = s(4).trim.split(" ").last.toInt 
    val next = s(2).trim match {
      case s"Test: divisible by $div" => (x: Long) => 
        if (x % div.toLong) == 0 then optionTrue else optionFalse
    }
    new Monkey(items, op, next)

@main 
def Main: Unit = 
  val lines =
    if DEBUG then
      io.Source.fromFile("test.txt").getLines.toSeq
    else
      io.Source.fromFile("input.txt").getLines.toSeq
  
  println(lines.solveOne)
  println(lines.solveTwo)
