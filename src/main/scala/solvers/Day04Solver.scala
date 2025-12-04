object Day04Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val grid = Grid2.fromLines(input)
    val heatMap = grid.mapWithIndices((pos, value) => {
      if (value == '.') {
        0
      } else {
        val fullNeighbours = grid
          .allNeighbors(pos)
          .filter(neighborPos => grid(neighborPos) == '@')
        if (fullNeighbours.length < 4) 1 else 0
      }
    })

    heatMap.sum.toString
  }

  override def solveSecondStar(input: List[String]): String = {
    val grid = Grid2.fromLines(input)
    calculateRemoved(grid).toString()
  }

  def calculateRemoved(grid: Grid2[Char]): Int = {
    val heatMap = grid.mapWithIndices((pos, value) => {
      if (value == '.') {
        -1
      } else {
        grid
          .allNeighbors(pos)
          .filter(neighborPos => grid(neighborPos) == '@')
          .length
      }
    })
    val removed = heatMap.filter(v => v < 4 && v >= 0).length

    if (removed == 0) {
      0
    } else {
      val nextGrid =
        grid.mapWithIndices((pos, value) =>
          if (heatMap(pos) < 4) '.' else value
        )
      removed + calculateRemoved(nextGrid)
    }
  }

  override def expectedFirstStar(): String = "13"

  override def expectedSecondStar(): String = "43"
}
