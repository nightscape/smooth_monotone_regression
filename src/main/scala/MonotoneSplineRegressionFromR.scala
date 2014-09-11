import breeze.linalg._

class MonotoneSplineRegressionFromR(knots: Knots, monotonicity: Int = 1) {
  val splines = new ISplineFromR(knots)
  val optimizer = new HingeQuadraticOptimization
  def learn(xs: DenseVector[Double], ys: DenseVector[Double]): Double => Double = {
    val edges = splines.monotoneIncreasing(xs)
    val amat = diag(DenseVector.fill[Double](knots.size + 1, monotonicity))
    amat.update(0, 0, 0.0)
    val qmat = edges * edges.t
    val cvec = edges * ys
    val coefficients = optimizer.quadProg(cvec, qmat, amat)
    val f = { d: Double => splines(d).dot(coefficients) }
    f
  }
}