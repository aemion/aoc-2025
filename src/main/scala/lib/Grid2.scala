case class Grid2[T](width: Int, height: Int, data: Vector[T]) {
  require(
    data.length == width * height,
    s"Data length ${data.length} must equal width * height (${width * height})"
  )

  private def index(pos: Vector2): Int = pos.y * width + pos.x

  private def inBounds(pos: Vector2): Boolean =
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

  def apply(pos: Vector2): T = {
    require(
      inBounds(pos),
      s"Position $pos out of bounds (width: $width, height: $height)"
    )
    data(index(pos))
  }

  def updated(pos: Vector2, value: T): Grid2[T] = {
    require(
      inBounds(pos),
      s"Position $pos out of bounds (width: $width, height: $height)"
    )
    Grid2(width, height, data.updated(index(pos), value))
  }

  def map[U](f: T => U): Grid2[U] =
    Grid2(width, height, data.map(f))

  def sum(implicit num: Numeric[T]): T =
    data.sum

  def foreach(f: T => Unit): Unit =
    data.foreach(f)

  def withIndices: Iterator[(Vector2, T)] =
    for {
      y <- (0 until height).iterator
      x <- (0 until width).iterator
      pos = Vector2(x, y)
    } yield (pos, apply(pos))

  def mapWithIndices[U](f: (Vector2, T) => U): Grid2[U] = {
    val newData = withIndices.map { case (pos, value) =>
      f(pos, value)
    }.toVector
    Grid2(width, height, newData)
  }

  override def toString: String = {
    (0 until height)
      .map { y =>
        (0 until width)
          .map { x =>
            apply(Vector2(x, y)).toString
          }
          .mkString("")
      }
      .mkString("\n")
  }

  def filter(f: T => Boolean): Vector[T] = {
    data.filter(f)
  }

  def allNeighbors(pos: Vector2) = Direction.allNeighbors(pos).filter(inBounds)
}

object Grid2 {
  def fill[T](width: Int, height: Int)(value: T): Grid2[T] =
    Grid2(width, height, Vector.fill(width * height)(value))

  def tabulate[T](width: Int, height: Int)(f: Vector2 => T): Grid2[T] = {
    val data = for {
      y <- 0 until height
      x <- 0 until width
    } yield f(Vector2(x, y))
    Grid2(width, height, data.toVector)
  }

  def fromLines(lines: List[String]): Grid2[Char] = parseLines(lines)(identity)

  def parseLines[T](
      lines: List[String]
  )(parse: Char => T): Grid2[T] = {
    require(lines.nonEmpty, "Lines must not be empty")
    val height = lines.length
    val width = lines.head.length
    require(
      lines.forall(_.length == width),
      "All lines must have the same length"
    )

    val data = lines.flatMap(_.map(parse)).toVector
    Grid2(width, height, data)
  }
}
