extension (vals: Seq[Int]) 
  def solve1: Long = decrypt(1L, 1)

  def solve2: Long = decrypt(811589153L, 10)

  private def decrypt(key: Long, rounds: Int): Long =
    val mixed = collection.mutable.ArrayBuffer.from(vals.map(_ * key).zipWithIndex)
    for _ <- 1 to rounds; index <- vals.indices do
      val from = mixed.indexWhere(_._2 == index)
      val pair @ (number, _) = mixed.remove(from)
      val remainder = (number % mixed.size).toInt
      val to = (from + remainder + mixed.size) % mixed.size
      mixed.insert(to, pair)
    end for
    val start = mixed.indexWhere(_._1 == 0)
    (1 to 3).map(offset => mixed((start + 1000 * offset) % mixed.size)._1).sum

@main 
def Main: Unit = 
  val vals = io.Source.fromFile("input.txt").getLines.toSeq.map(_.toInt)

  println(vals.solve1)
  println(vals.solve2)