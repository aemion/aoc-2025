import scala.collection.mutable

def memoize[K,R](k: K)(f: => R)(implicit m: mutable.Map[K,R]) = m.getOrElseUpdate(k, f)


object Day07Solver extends DaySolver {
  implicit val memoMap: mutable.HashMap[String,Long] = mutable.HashMap[String,Long]()

  override def solveFirstStar(input: List[String]): String = {
    val beams = Set(input.head.indexWhere(_ == 'S'))
    
    val splitters = parseInput(input)
    advanceAndCountSplits(beams, splitters).toString()
  }

  def parseInput(input: List[String]) = {
    input
      .map(
        _.zipWithIndex
          .filter((c: Char, n: Int) => c == '^')
          .map((_, n) => n)
          .toSet
      )
      .filter(!_.isEmpty)
  }

  def advanceAndCountSplits(beams: Set[Int], allSplitters: List[Set[Int]]): Int = {
    allSplitters match
      case Nil => 0
      case splitters :: rest => {
        val untouchedBeams = beams.diff(splitters)
        val splittedBeams = beams.intersect(splitters)
        val toAdd = splittedBeams.size
        splittedBeams.size + advanceAndCountSplits(
          untouchedBeams.union(splittedBeams.map(b => Set(b - 1, b + 1)).flatten),
          rest
        )
      }
  }

  override def solveSecondStar(input: List[String]): String = {
    val beam = input.head.indexWhere(_ == 'S')

    val splitters = parseInput(input)


    (1 + advanceAndCountPaths(beam, splitters)).toString()
  }


  def advanceAndCountPaths(beam: Int, allSplitters: List[Set[Int]]): Long = memoize(s"${beam}|${allSplitters.size}") {
    allSplitters match
      case Nil => 0
      case splitters :: rest => {
        if (!splitters.contains(beam)) {
          advanceAndCountPaths(beam, rest)
        } else {
          1 + advanceAndCountPaths(beam - 1, rest) + advanceAndCountPaths(beam + 1, rest)
        }
      }
  }

  override def expectedFirstStar(): String = "21"

  override def expectedSecondStar(): String = "40"
}
