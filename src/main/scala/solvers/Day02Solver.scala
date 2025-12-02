object Day02Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val idRanges = input.head.split(",")

    idRanges.map(calculateForRange(_, isRepeatingTwice)).sum.toString()
  }

  def calculateForRange(input: String, isInvalid: Long => Boolean): Long = {
    val range = input match
      case s"${start}-${end}" => start.toLong to end.toLong

    range.filter(isInvalid).sum
  }

  def isRepeatingTwice(n: Long): Boolean = isRepeatingNTimes(n, 2)

  def isRepeatingNTimes(number: Long, n: Int) = {
    val str = number.toString()
    val length = str.length()

    (length % n == 0) && isSame(str.grouped(length / n)) 
  }

  def isSame(splitString: Iterator[String]): Boolean = splitString.distinct.size == 1
  
  def isRepeatingAtLeastTwice(n: Long): Boolean = {
    val str = n.toString()
    val range = 2 to str.length

    range.exists(isRepeatingNTimes(n, _))
  }

  override def solveSecondStar(input: List[String]): String = {
    val idRanges = input.head.split(",")

    idRanges.map(calculateForRange(_, isRepeatingAtLeastTwice)).sum.toString()
  }

  override def expectedFirstStar(): String = "1227775554"

  override def expectedSecondStar(): String = "4174379265"
}
