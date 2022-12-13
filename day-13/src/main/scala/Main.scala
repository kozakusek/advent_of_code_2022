extension (p: (String, String))
  def matches: Boolean = p match { case (left, right) => (left.head, right.head) match 
    case (a, b) if a == b => (left.tail, right.tail).matches
    case (']', _) => true
    case (_, ']') => false
    case ('[', b) => (left.tail, s"$b]${right.tail}").matches
    case (a, '[') => (s"$a]${left.tail}", right.tail).matches
    case (a, b) => a < b
  }
    
extension (s: Seq[String])
  def cleanup: Seq[String] = s.filter(_.nonEmpty).map(_.replace("10", "A"))

  def solveOne: Int = s
    .cleanup
    .grouped(2)
    .zipWithIndex
    .map((pair, index) => if ((pair(0), pair(1))).matches then index + 1 else 0)
    .sum
  
  def solveTwo: Int = s
    .cleanup
    .toSet
    .incl("[[2]]")
    .incl("[[6]]")
    .toSeq
    .sortWith((a, b) => (a, b).matches)
    .zipWithIndex
    .filter((packet, index) => packet == "[[2]]" || packet == "[[6]]")
    .map(_._2 + 1)
    .reduce(_ * _)


@main
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solveOne)
  println(lines.solveTwo)
