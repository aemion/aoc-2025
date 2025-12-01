import scala.io.Source

object InputLoader {
  def load(day: Int, test: Boolean): List[String] = {
    val fileName = if (test) s"day${day}_test.txt" else s"day${day}.txt"
    val source = Source.fromFile(s"resources/inputs/$fileName")
    try source.getLines().toList
    finally source.close()
  }
}

