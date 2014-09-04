trait Knots extends IndexedSeq[Double] {
  def intervalOf(x: Double): Interval
}