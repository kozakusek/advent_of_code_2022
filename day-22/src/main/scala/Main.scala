type Point = (Int, Int)
type State = (Point, Point)
enum Rotation { case Clockwise, CounterClockwise }
enum Tile { case Open, Solid, Wrap }

extension (p: Point)
  def +(q: Point): Point = (p._1 + q._1, p._2 + q._2)
  def rotate(r: Rotation): Point = r match
    case Rotation.Clockwise => (-p._2, p._1)
    case Rotation.CounterClockwise => (p._2, -p._1)

val right: Point = (1, 0)
val down: Point = (0, 1)
val left: Point = (-1, 0)
val up: Point = (0, -1)

extension (s: Seq[String])
  def solve1: Int = 
    val (tiles, path) = s.parse
    val minX = tiles.keys.groupMapReduce(_._2)(_._1)(_ min _)
    val maxX = tiles.keys.groupMapReduce(_._2)(_._1)(_ max _)
    val minY = tiles.keys.groupMapReduce(_._1)(_._2)(_ min _)
    val maxY = tiles.keys.groupMapReduce(_._1)(_._2)(_ max _)

    def handler(pos: Point, dir: Point): State = dir match
      case `right` => pos.copy(_1 = minX(pos._2)) -> right
      case `left` => pos.copy(_1 = maxX(pos._2)) -> left
      case `down` => pos.copy(_2 = minY(pos._1)) -> down
      case `up` => pos.copy(_2 = maxY(pos._1)) -> up
    
    follow(path, tiles, handler)

  def solve2: Int = 
    val (tiles, path) = s.parse

    def handleWrap(pos: Point, dir: Point): State =
      val (cubeX, cubeY) = (pos._1 / 50, pos._2 / 50)
      val (modX, modY) = (pos._1 % 50, pos._2 % 50)
      (cubeX, cubeY, dir) match
        case (2, 0, `up`) => (modX, 199) -> up          
        case (2, 0, `down`) => (99, 50 + modX) -> left  
        case (2, 0, `right`) => (99, 149 - modY) -> left
        case (1, 0, `up`) => (0, 150 + modX) -> right   
        case (1, 0, `left`) => (0, 149 - modY) -> right 
        case (1, 1, `left`) => (modY, 100) -> down      
        case (1, 1, `right`) => (100 + modY, 49) -> up  
        case (1, 2, `down`) => (49, 150 + modX) -> left 
        case (1, 2, `right`) => (149, 49 - modY) -> left
        case (0, 2, `up`) => (50, 50 + modX) -> right   
        case (0, 2, `left`) => (50, 49 - modY) -> right 
        case (0, 3, `down`) => (100 + modX, 0) -> down  
        case (0, 3, `left`) => (50 + modY, 0) -> down   
        case (0, 3, `right`) => (50 + modY, 149) -> up  
    
    follow(path, tiles, handleWrap)

  private def parse: (Map[Point, Tile], String) =
    val points = for
      (row, y) <- s.dropRight(2).zipWithIndex
      (cell, x) <- row.zipWithIndex
      if cell != ' '
    yield (x, y) -> (if cell == '.' then Tile.Open else Tile.Solid)
    (points.toMap.withDefaultValue(Tile.Wrap), s.last)

  private def follow(path: String, tiles: Map[Point, Tile], handler: State => State): Int =
    val numbers = path.split("\\D+").map(_.toInt).toSeq
    val letters = path.split("\\d+").toSeq
    val moves = numbers.zip(letters)
    val start = ((50, 0), (1, 0))
    val (pos, dir) = moves.foldLeft(start) { case ((pos, dir), (number, letter)) =>
      val nextDir = letter match
        case "L" => dir.rotate(Rotation.CounterClockwise)
        case "R" => dir.rotate(Rotation.Clockwise)
        case _ => dir
      (1 to number).foldLeft((pos, nextDir)) { case ((pos, dir), _) =>
        val next = pos + dir
        tiles(next) match
          case Tile.Open => (next, dir)
          case Tile.Solid => (pos, dir)
          case Tile.Wrap =>
            val (wrapPos, wrapDir) = handler(pos, dir)
            if tiles(wrapPos) == Tile.Open then (wrapPos, wrapDir) else (pos, dir)
      }
    }
    1000 * (pos._2 + 1) + 4 * (pos._1 + 1) + Seq(right, down, left, up).indexOf(dir)

@main
def Main: Unit = 
  val lines = io.Source.fromFile("input.txt").getLines.toSeq

  println(lines.solve1)
  println(lines.solve2)