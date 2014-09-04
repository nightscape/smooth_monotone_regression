import scala.math._

class MSpline(knots: Knots) {
  def translateIndex(index: Int, order: Int, cap: Int) = {
    min(max(index - (order - 1), 0), cap)
  }
  def apply(order: Int, i: Int, x: Double): Double = order match {
    case 1 =>
      val Interval(index, left, right) = knots.intervalOf(x)
      if (i == index)
        1.0 / (right - left)
      else
        0.0
    case n =>
      val leftSplineIndex = max(0, i - 1)
      val rightSplineIndex = min(i, knots.length - 4 + order)
      val tLeft = knots(translateIndex(i, order, Int.MaxValue))
      val tRight = knots(translateIndex(i + order, order, knots.length - 1))
      val leftSummand = if (i == 0) 0 else (x - tLeft) * apply(order - 1, i - 1, x)
      val rightSummand = if (i == knots.length - 3 + order) 0 else (tRight - x) * apply(order - 1, i, x)
      (order
        * (leftSummand
          + rightSummand)
          / (order - 1)
          / (tRight - tLeft))
  }
}
