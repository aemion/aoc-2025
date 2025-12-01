import caseapp._

object App extends CaseApp[Config] {
  def run(config: Config, remainingArgs: RemainingArgs): Unit = {

    println(s"AOC 2025 - Day ${config.day}")
    if config.test then
      println("Running in TEST mode...")
    SolverRegistry.get(config.day) match {
      case Some(solver) =>
        val input = InputLoader.load(config.day, config.test)

        println(s"Day ${config.day} - Part 1:")
        val part1Result = solver.solveFirstStar(input)
        println(part1Result)


        if (config.test) {
          val expected1 = solver.expectedFirstStar()
          assert(part1Result == expected1, s"Part 1 failed: got $part1Result, expected $expected1")

          println("✅ First star test passed!")

        }

        println(s"Day ${config.day} - Part 2:")
        val part2Result = solver.solveSecondStar(input)
        println(part2Result)

        if (config.test) {
          val expected2 = solver.expectedSecondStar()
          assert(part2Result == expected2, s"Part 2 failed: got $part2Result, expected $expected2")
          
          println("✅ Second star test passed!")
        }

      case None =>
        println(s"No solver found for day ${config.day}")
    }
  }
}
