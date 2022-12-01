@main def First() : Unit = {
    val lines = scala.io.Source.fromFile("input-1.txt").getLines()

    var max = 0
    var elf = 0
    
    for (line <- lines) {
        if line.isEmpty() then
            max = max.max(elf)
            elf = 0
        else
            elf += line.toInt
    }

    println(max.max(elf))
}