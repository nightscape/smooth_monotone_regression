import scala.math._

class ISpline(knots: Knots) {
  val mSpline = new MSpline(knots)
  def apply(order: Int, i: Int, x: Double): Double = {
    val Interval(index, left, right) = knots.intervalOf(x)
    val j = index + order
    if (i > j) {
      0.0
    } else if (j - order > i) {
      1.0
    } else {
      val summands = (i to j).map { m =>
        val tLeft = knots(max(m - order + 1, 0))
        val tRight = knots(min(m + 1, knots.length - 1))
        require(tLeft <= tRight)
        (tRight - tLeft) * mSpline(order + 1, m - 1, x)
      }
      summands.sum / (order - 1)
    }
  }
}