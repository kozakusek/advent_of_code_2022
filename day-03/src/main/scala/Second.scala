import scala.io.Source

def solve2(lines: Seq[String]) =
    lines
        .grouped(3)
        .map({ case Seq(x, y, z) => x.intersect(y).intersect(z) })
        .map(x => priority(x.head))
        .sum
        
@main
def Second() =
    val lines = Source.fromFile("input.txt").getLines.toSeq
    println(solve2(lines))
