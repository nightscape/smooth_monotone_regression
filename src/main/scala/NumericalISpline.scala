  class NumericalISpline(knots: Knots) {
    val mSpline = new MSpline(knots)
    def apply(order: Int, i: Int, x: Double): Double = {
      val Interval(index, left, right) = knots.intervalOf(x)
      val j = index + order
      if (i > j) {
        0.0
      } else if (j - order > i) {
        1.0
      } else {
        val step = (x - knots(0)) / 2048
        if (step < 0.0000001)
          0
        else
          (knots(0) to x by step).map { v => mSpline(order + 1, i, v) }.sum * step
      }
    }
  }