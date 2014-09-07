class ArbitrarilyDistributedKnots(xs: Seq[Double]) extends Knots {
  val xSorted = xs.sorted.toVector
  def intervalOf(x: Double) = {
    require(xSorted.head <= x)
    require(x <= xSorted.last)
    val index = xSorted.sliding(2).zipWithIndex.collectFirst { case (Vector(lower, upper), index) if lower <= x && x <= upper => index }.get
    Interval(index, this(index), this(index + 1))
  }
  override def apply(index: Int): Double = {
    require(0 <= index)
    require(index < this.length)
    xSorted(index)
  }
  override val length = xSorted.length
}