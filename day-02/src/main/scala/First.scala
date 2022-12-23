enum Move:
    case Rock, Paper, Scissors

enum RoundResult:
    case Win, Lose, Draw

def textToMove(text: String) = text match
    case "A" => Move.Rock
    case "B" => Move.Paper
    case "C" => Move.Scissors
    case "X" => Move.Rock
    case "Y" => Move.Paper
    case "Z" => Move.Scissors

def moveValue(move: Move) = move match
    case Move.Rock => 1
    case Move.Paper => 2
    case Move.Scissors => 3

def getResult(move1: Move, move2: Move) = (move1, move2) match
    case (Move.Rock, Move.Paper) => RoundResult.Lose
    case (Move.Rock, Move.Scissors) => RoundResult.Win
    case (Move.Paper, Move.Rock) => RoundResult.Win
    case (Move.Paper, Move.Scissors) => RoundResult.Lose
    case (Move.Scissors, Move.Rock) => RoundResult.Lose
    case (Move.Scissors, Move.Paper) => RoundResult.Win
    case _ => RoundResult.Draw

val PointsForWin = 6
val PointsForDraw = 3

def getScores(line: String) : (Int, Int) = {
    val elems = line.split(" ")
    val oppMove = textToMove(elems(0))
    val myMove = textToMove(elems(1))

    var oppScore = moveValue(oppMove)
    var myScore = moveValue(myMove)

    getResult(myMove, oppMove) match
        case RoundResult.Win => myScore += PointsForWin
        case RoundResult.Lose => oppScore += PointsForWin
        case RoundResult.Draw => 
            myScore += PointsForDraw
            oppScore += PointsForDraw
    
    
    (oppScore, myScore)
}

@main def First() : Unit = {
  val lines = scala.io.Source.fromFile("input-1.txt").getLines()

  var oppScore = 0
  var myScore = 0

  for line <- lines do
    val (opp, my) = getScores(line)
    oppScore += opp
    myScore += my

  println(s"Opponent: $oppScore")
  println(s"Me: $myScore")
}