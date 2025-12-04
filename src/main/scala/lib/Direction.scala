enum Direction(val delta: Vector2) {
  case Top         extends Direction(Vector2(-1,  0))
  case Right       extends Direction(Vector2( 0,  1))
  case Bottom      extends Direction(Vector2( 1,  0))
  case Left        extends Direction(Vector2( 0, -1))
  case TopRight    extends Direction(Vector2(-1,  1))
  case BottomRight extends Direction(Vector2( 1,  1))
  case BottomLeft  extends Direction(Vector2( 1, -1))
  case TopLeft     extends Direction(Vector2(-1, -1))
}

object Direction { 
  val cardinals: List[Direction] = List(Direction.Top, Direction.Right, Direction.Bottom, Direction.Left)
  val diagonals: List[Direction] = List(Direction.TopRight, Direction.BottomRight, Direction.BottomLeft, Direction.TopLeft)
  val all: List[Direction]      = cardinals ++ diagonals

  def orthogonalNeighbors(p: Vector2): List[Vector2] = cardinals.map(d => p + d.delta)
  def allNeighbors(p: Vector2): List[Vector2] = all.map(d => p + d.delta)
}
