object Day01Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val start = 50
    val instructions = input.map(split)
    val (numberOfZeros, previous) = instructions.foldLeft((0, start)) {
      case ((accNumberOfZeros, acc), instruction) =>  
        val nextNumber = ((acc + instruction) % 100 + 100) % 100

        val nextNumberOfZeros = accNumberOfZeros + (if (nextNumber == 0) 1 else 0)
        (nextNumberOfZeros, nextNumber)
    }
    
    numberOfZeros.toString
  }

  def split(instruction: String): Int = 
    instruction match
      case s"L${value}" => -value.toInt
      case s"R${value}" => value.toInt

  override def solveSecondStar(input: List[String]): String = {
    val start = 50
    val instructions = input.map(split)
    val (numberOfZeros, previous) = instructions.foldLeft((0, start)) {
      case ((accNumberOfZeros, acc), instruction) =>  
        val nextWithoutModulo = (acc + instruction)
        val toAdd = Math.floorDiv(Math.abs(nextWithoutModulo), 100) + (if (nextWithoutModulo <= 0 && acc != 0) 1 else 0)
        val nextNumber = ((nextWithoutModulo) % 100 + 100) % 100

        val nextNumberOfZeros = accNumberOfZeros + toAdd 
        (nextNumberOfZeros, nextNumber)
    }

    numberOfZeros.toString
  }

  override def expectedFirstStar(): String = "3"
  
  override def expectedSecondStar(): String = "6"
}
