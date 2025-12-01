object SolverRegistry {
  private val solvers: Map[Int, DaySolver] = Map(
    1 -> Day01Solver,
    2 -> Day02Solver,
    3 -> Day03Solver,
    4 -> Day04Solver,
    5 -> Day05Solver,
    6 -> Day06Solver,
    7 -> Day07Solver,
    8 -> Day08Solver,
    9 -> Day09Solver,
    10 -> Day10Solver,
    11 -> Day11Solver,
    12 -> Day12Solver,
  )

  def get(day: Int): Option[DaySolver] = solvers.get(day)
}

