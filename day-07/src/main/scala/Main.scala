enum Part {
  case One, Two
}

type Path = List[String]
val root : Path = List("/")

val totalSize : Int = 70000000
val sizeToFree : Int = 30000000
val smallDirSizeLimit : Int = 100000

extension (s: Map[Path, Int])
  def update(path: Path, change: Int): Map[Path, Int] = path match
    case Nil => s
    case _ :: tail => s.update(tail, change).updated(path, s(path) + change)

extension (s: Seq[String])
  def solve(part: Part): Int = part match
    case Part.One => s.buildDirTree.filter(_ <= smallDirSizeLimit).sum
    case Part.Two => 
      val dirTree = s.buildDirTree
      val req = sizeToFree - (totalSize - dirTree.max)
      dirTree.filter(_ >= req).min
    
  private def buildDirTree: Seq[Int] = 
    s.foldLeft((root, Map(root -> 0))) {
      case ((path, sizes), instr) => instr match
        case "$ ls" => (path, sizes.update(path, -sizes(path)))
        case "$ cd /" => (root, sizes)
        case "$ cd .." => (path.tail, sizes)
        case s"$$ cd $name" => (name :: path, sizes.updated(name :: path, 0))
        case s"dir $name" => (path, sizes)
        case s"$size $name" => (path, sizes.update(path, size.toInt)) 
    }
    ._2.values.toSeq

@main
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq
  
  println(lines.solve(Part.One))
  println(lines.solve(Part.Two))
