import SeqExtension._

object Day06Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val splitInput = input.map(_.split(" ").filter(!_.isEmpty))
    val operators :: rawNumbers = splitInput.reverse
    val numbers = rawNumbers.transpose

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
    val numbers = input
      .dropRight(1)
      .map(_.split(""))
      .transpose
      .split(isEmptyList)
      .map(l => l.map(_.mkString("").trim.toLong))

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
