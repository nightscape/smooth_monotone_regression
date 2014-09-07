class UniformlyDistributedKnots(left: Double, right: Double, numKnots: Int) extends Knots {
  private val intervalWidth = (right - left) / (numKnots - 1)
  def intervalOf(x: Double) = {
    require(left <= x)
    require(x <= right)
    val index = scala.math.min(numKnots - 2, ((x - left) / intervalWidth).toInt)
    Interval(index, this(index), this(index + 1))
  }
  override def apply(index: Int): Double = {
    require(0 <= index)
    require(index < numKnots)
    left + index * intervalWidth
  }
  override val length = numKnots
}