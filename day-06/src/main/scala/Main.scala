import scala.util.control.Breaks._

extension (stream: Array[Char])
  def solve(mark: Int): Int = 
    var result = -1
    breakable {
      for i <- mark until stream.length do 
        if stream.slice(i - mark, i).toSet.size == mark then 
          result = i
          break
    }
    result

@main 
def Main: Unit = 
  val stream = io.Source.fromFile("input.txt").iter.toArray
  println(stream.solve(4))
  println(stream.solve(14))

