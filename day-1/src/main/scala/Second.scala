import collection.mutable.PriorityQueue

@main def Second() : Unit = {
    val lines = scala.io.Source.fromFile("input-2.txt").getLines()

    var calories: PriorityQueue[Int] = PriorityQueue()
    var elf = 0
    
    for (line <- lines) {
        if line.isEmpty() then
            calories += elf
            elf = 0
        else
            elf += line.toInt
    }
    calories += elf

    println(calories.dequeue() + calories.dequeue() + calories.dequeue())
}