import scala.io.Source

object InputLoader {
  def load(day: Int, test: Boolean): List[String] = {
    val fileName = if (test) s"test_inputs/day${day}.txt" else s"inputs/day${day}.txt"
    val source = Source.fromFile(s"resources/$fileName")
    try source.getLines().toList
    finally source.close()
  }
}

