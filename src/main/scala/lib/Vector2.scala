case class Vector2(x: Int, y: Int) {
  def +(that: Vector2) = Vector2(x + that.x, y + that.y)

  def *(n: Int) = Vector2(x * n, y * n)

  override def toString() = s"(${x},${y})"
}
