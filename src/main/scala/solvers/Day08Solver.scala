case class Vector3(x: Long, y: Long, z: Long) {
  def squaredDistance(that: Vector3): Long =
    (this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y) + (this.z - that.z) * (this.z - that.z)
}

case class Segment(a: Vector3, b: Vector3, squaredDistance: Long)
    extends Ordered[Segment] {
  def compare(that: Segment): Int =
    this.squaredDistance compare that.squaredDistance
}

object Day08Solver extends DaySolver {
  override def solveFirstStar(input: List[String]): String = {
    val iterationsStr :: coordinates = input
    val iterations = iterationsStr.toInt
    val points = coordinates.map(_ match {
      case s"${x},${y},${z}" => Vector3(x.toLong, y.toLong, z.toLong)
    })

    val segments = calculateSegments(points).sorted
      .slice(0, iterations)

    val connectedSegments = connect(segments)
      .sortWith((a, b) => a.size > b.size)
      .slice(0, 3)

    connectedSegments.map(_.size).product.toString()
  }

  def connectSegment(
      seg: Segment,
      rest: List[Set[Vector3]]
  ): List[Set[Vector3]] = {
    val firstSet = rest.find(_.contains(seg.a))
    val secondSet = rest.find(_.contains(seg.b))

    (firstSet, secondSet) match {
      case (None, None)      => Set(seg.a, seg.b) :: rest
      case (None, Some(set)) => rest.updated(rest.indexOf(set), set + seg.a)
      case (Some(set), None) => rest.updated(rest.indexOf(set), set + seg.b)
      case (Some(setA), Some(setB)) =>
        if (setA == setB) {
          rest
        } else {
          rest
            .updated(rest.indexOf(setA), setA ++ setB)
            .diff(List(setB))
        }
    }
  }

  def connect(segments: List[Segment]): List[Set[Vector3]] = {
    segments match {
      case Nil         => List()
      case seg :: Nil  => List(Set(seg.a, seg.b))
      case seg :: rest => connectSegment(seg, connect(rest))
    }
  }

  def calculateSegments(coords: List[Vector3]): List[Segment] = {
    coords match {
      case Nil          => List()
      case point :: Nil => List()
      case head :: rest =>
        rest.map(other =>
          Segment(head, other, head.squaredDistance(other))
        ) ::: calculateSegments(rest)
    }
  }

  override def solveSecondStar(input: List[String]): String = {
    val points = input
      .drop(1)
      .map(_ match {
        case s"${x},${y},${z}" => Vector3(x.toLong, y.toLong, z.toLong)
      })

    val segments = calculateSegments(points).sorted
    val segmentThatConnectsAll = connectUntilFinished(segments, points.size)

    (segmentThatConnectsAll.a.x * segmentThatConnectsAll.b.x).toString()
  }

  def connectUntilFinished(
      segments: List[Segment],
      numberOfPoints: Int
  ): Segment = {
    def inner(
        segments: List[Segment],
        acc: List[Set[Vector3]],
        expectedSize: Int
    ): Segment = {
      segments match {
        case seg :: Nil  => seg
        case seg :: next => {
          val nextSegments = connectSegment(seg, acc)
          if (
            nextSegments.size == 1 && nextSegments.head.size == expectedSize
          ) {
            seg
          } else {
            inner(next, nextSegments, expectedSize)
          }
        }
      }
    }

    inner(segments, List(), numberOfPoints)
  }

  override def expectedFirstStar(): String = "40"

  override def expectedSecondStar(): String = "25272"
}
