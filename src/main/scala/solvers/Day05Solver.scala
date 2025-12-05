case class Interval(min: Long, max: Long) extends Ordered[Interval] {
  require(min <= max, s"Min ${min} must be lower than max ${max}")

  def contains(x: Long) = x >= min && x <= max
  def compare(that: Interval): Int = this.min compare that.min

  // Intervals must be "mergeable" i.e. they must be overlapping
  def merge(i: Interval): Interval = i match {
    case Interval(min, max) if min >= this.min && max <= this.max   => this
    case Interval(min, max) if this.contains(min) && max > this.max =>
      Interval(this.min, max)
    case Interval(min, max) if min < this.min && this.contains(max) =>
      Interval(min, this.max)
    case Interval(min, max) if min < this.min && max > this.max => i
  }

  def count(): Long = max - min + 1
}

object Day05Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val (ranges, numbers) = parseInput(input)
    numbers.filter(n => ranges.exists(r => r.contains(n))).length.toString()
  }

  def parseInput(input: List[String]) = {
    input.filter(_.nonEmpty) partitionMap {
      case s"${min}-${max}" => Left(Interval(min.toLong, max.toLong))
      case x: String        => Right(x.toLong)
    }
  }

  override def solveSecondStar(input: List[String]): String = {
    val (ranges, numbers) = parseInput(input)

    val intervals = ranges.sorted
      .foldLeft(List.empty[Interval]) { (acc, interval) =>
        acc match {
          case head :: tail if head.contains(interval.min) =>
            head.merge(interval) :: tail
          case _ =>
            interval :: acc
        }
      }
      .reverse
    intervals.map(_.count()).sum.toString()
  }

  override def expectedFirstStar(): String = "3"

  override def expectedSecondStar(): String = "14"
}
