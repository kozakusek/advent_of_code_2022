import scala.io.Source

def priority(item: Char) =
    if item.isLower then item - 'a' + 1
    else item - 'A' + 27

def solve1(lines: Seq[String]) =
    lines
        .map(x => x.splitAt(x.length / 2))
        .map((x, y) => x.intersect(y))
        .map(x => priority(x.head))
        .sum
        
@main
def First() =
    val lines = Source.fromFile("input.txt").getLines.toSeq
    println(solve1(lines))
