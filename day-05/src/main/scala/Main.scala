import scala.io.Source
import scala.collection.mutable.Stack
import scala.util.matching.Regex

enum Task {
  case One
  case Two
}

val instrPattern : Regex = "move (\\d+) from (\\d+) to (\\d+)".r

def dataToStacks(data: Seq[String]) : Array[Stack[Char]] = 
  var result = Array.ofDim[Stack[Char]](data.last.split("\\s+").length - 1)
  for (i <- 0 until result.length) result(i) = Stack[Char]()
  for line <- data.reverse.tail do
    for i <- 0 until result.length do
      val box = line.slice(4 * i, 4 * i + 3).trim
      if box != "" then result(i).push(box(1))
  result

extension (ss: Seq[String])
  def solve(task: Task, stacks: Array[Stack[Char]]) : String =
    task match
      case Task.One => solve(stacks, false)
      case Task.Two => solve(stacks, true)
  
  private def solve(stacks: Array[Stack[Char]], rev: Boolean) : String = 
    ss.map(instr => instr match 
      case instrPattern(count, from, to) => (count.toInt, from.toInt - 1, to.toInt - 1)
      case _ => throw new Exception("Invalid instruction: " + instr)
    ).foldLeft(stacks)((stacks , instr) => 
      val (count, from, to) = instr
      val move = ((x: Stack[Char]) => if rev then x.reverse else x) (stacks(from).take(count))
      stacks.zipWithIndex.map((s, i) => 
        if i == to then s.pushAll(move) 
        else if i == from then s.drop(count)
        else s)
    ).map(_.headOption
    ).map(_.getOrElse(' ')
    ).mkString
    
@main 
def Main: Unit =
  val (data, input) = Source.fromFile("input.txt").getLines().toSeq.splitAt(lines.indexOf(""))

  println(input.tail.solve(Task.One, dataToStacks(data)))
  println(input.tail.solve(Task.Two, dataToStacks(data)))
