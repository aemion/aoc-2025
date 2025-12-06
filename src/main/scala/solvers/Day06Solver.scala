object Day06Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val splitInput = input.map(_.split(" ").filter(!_.isEmpty).toVector)
    val operators :: rawNumbers = splitInput.reverse
    val numbers = rawNumbers.transpose.toVector

    val colResults = operators.zipWithIndex.map((operator: String, i: Int) => {
      val ns = numbers(i).map(_.toLong)
      operator match
        case "+" => ns.sum
        case "*" => ns.product
    })
    
    colResults.sum.toString()
  }

  def isEmptyList(l: List[String]) = l.forall(_ == " ")

  override def solveSecondStar(input: List[String]): String = {
    val withoutOperators = input.dropRight(1)

    val transposedInput = withoutOperators
      .map(_.split("").toList)
      .transpose
    val emptyIndexes = transposedInput.zipWithIndex.collect{ case (a, index) if (isEmptyList(a)) => index}
    
    val partitions = (emptyIndexes.head + 1) :: (emptyIndexes, emptyIndexes drop 1).zipped.map((a, b) => b - a)
    val itr = transposedInput.iterator
    val numbers = partitions
      .map(itr.take(_).toList.filter(!isEmptyList(_)))
      .toList
      .map(l => l.map(_.filter(_ != " ").mkString("").toLong))

    val operators = input.reverse.head.split(" ").filter(!_.isEmpty)

    val colResults = operators.zipWithIndex.map((operator: String, i: Int) => {
      operator match
        case "+" => numbers(i).sum
        case "*" => numbers(i).product
    })

    colResults.sum.toString()
  }

  override def expectedFirstStar(): String = "4277556"

  override def expectedSecondStar(): String = "3263827"
}
