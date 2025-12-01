object SolverRegistry {
  private val solvers: Map[Int, DaySolver] = Map(
    1 -> Day01Solver,
    // Add more days here
  )

  def get(day: Int): Option[DaySolver] = solvers.get(day)
}

