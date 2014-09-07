import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

class MonotoneSplineRegressionFromR(knots: DenseVector[Double]) {
  val optimizer = new HingeQuadraticOptimization
  def learn(xs: DenseVector[Double], ys: DenseVector[Double]): Double => Double = {
    val edges = ISplineFromR.monotoneIncreasing(xs, knots)
    val amat = DenseMatrix.eye[Double](knots.size + 1)
    amat.update(0, 0, 0.0)
    val qmat = edges * edges.t
    val cvec = edges * ys
    val coefficients = optimizer.quadProg(cvec, qmat, amat)
    val f = {d: Double =>
      val e = ISplineFromR.monotoneIncreasing(DenseVector(d), knots)
      val yhat = (e.t * coefficients)
      yhat.apply(0)
    }
    f
  }
}