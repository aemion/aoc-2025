object Day03Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = input.map(largestJoltage).sum.toString()

  def largestJoltage(input: String): Int = {
    val numbers = input.split("").map(_.toInt).toList
    
    val last = numbers.last
    val firstNumbers = numbers.dropRight(1)
    val firstResult = firstNumbers.foldLeft((0, 0))(twoGreatest(_, _, false)) 

    val result = twoGreatest(firstResult, last, true)
    result match 
      case (x, y) => s"$x$y".toInt
  }

  def twoGreatest(currentJoltage: (Int, Int), nextBattery: Int, isLast: Boolean) = currentJoltage match
    case (x, y) => if (nextBattery  > x && !isLast) (nextBattery, 0) else if (nextBattery > y) (x, nextBattery) else (x, y)
  

  override def solveSecondStar(input: List[String]): String = input.map(evenLargestJoltage).sum.toString()

  def evenLargestJoltage(input: String): Long = {
    val numbers = input.split("").map(_.toInt).toList

    val greatest = greatestNumbersReversed(numbers, 12, List()).reverse
    toLong(greatest)
  }

  def greatestNumbersReversed(numbers: List[Int], n: Int, current: List[Int]): List[Int] = {
    if (n == 0) {
      current
    } else {
      val candidates = numbers.slice(0, numbers.length - n + 1)
      val max = candidates.max
      val indexOfMax = candidates.indexOf(max)
      val nextNumbers = numbers.slice(indexOfMax + 1, numbers.length)
      greatestNumbersReversed(nextNumbers, n - 1, max :: current)
    }
  }

  def toLong(numbers: List[Int]): Long = if (numbers.isEmpty) 0 else numbers.map(_.toString()).mkString("").toLong

  override def expectedFirstStar(): String = "357"

  override def expectedSecondStar(): String = "3121910778619"
}
