def textToResult(text: String) = text match
    case "X" => RoundResult.Lose
    case "Y" => RoundResult.Draw
    case "Z" => RoundResult.Win

def moveFromResult(move : Move, result: RoundResult) = (move, result) match
    case (Move.Rock, RoundResult.Win) => Move.Paper
    case (Move.Rock, RoundResult.Lose) => Move.Scissors
    case (Move.Paper, RoundResult.Win) => Move.Scissors
    case (Move.Paper, RoundResult.Lose) => Move.Rock
    case (Move.Scissors, RoundResult.Win) => Move.Rock
    case (Move.Scissors, RoundResult.Lose) => Move.Paper
    case (x, RoundResult.Draw) => x

def getScores2(line: String) : (Int, Int) = {
    val elems = line.split(" ")
    val oppMove = textToMove(elems(0))
    val result = textToResult(elems(1))
    val myMove = moveFromResult(oppMove, result)

    var oppScore = moveValue(oppMove)
    var myScore = moveValue(myMove)

    result match
        case RoundResult.Win => myScore += PointsForWin
        case RoundResult.Lose => oppScore += PointsForWin
        case RoundResult.Draw => 
            myScore += PointsForDraw
            oppScore += PointsForDraw
    
    
    (oppScore, myScore)
}

@main def Second() : Unit = {
  val lines = scala.io.Source.fromFile("input-2.txt").getLines()

  var oppScore = 0
  var myScore = 0

  for line <- lines do
    val (opp, my) = getScores2(line)
    oppScore += opp
    myScore += my

  println(s"Opponent: $oppScore")
  println(s"Me: $myScore")
}