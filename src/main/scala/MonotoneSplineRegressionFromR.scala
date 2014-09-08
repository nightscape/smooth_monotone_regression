import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

class MonotoneSplineRegressionFromR(knots: Knots) {
  val splines = new ISplineFromR(knots)
  val optimizer = new HingeQuadraticOptimization
  def learn(xs: DenseVector[Double], ys: DenseVector[Double]): Double => Double = {
    val edges = splines.monotoneIncreasing(xs)
    val amat = DenseMatrix.eye[Double](knots.size + 1)
    amat.update(0, 0, 0.0)
    val qmat = edges * edges.t
    val cvec = edges * ys
    val coefficients = optimizer.quadProg(cvec, qmat, amat)
    val f = {d: Double =>
      val e = splines.monotoneIncreasing(DenseVector(d))
      val yhat = (e.t * coefficients)
      yhat.apply(0)
    }
    f
  }
}