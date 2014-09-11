import breeze.linalg._
import breeze.numerics._
import scala.annotation.tailrec
/**
 * quadratic programming code: hinge algorithm
 *  find theta to minimize t(theta) * qmat * theta - 2* theta * cvec
 *  subject to amat * theta >= 0
 *
 * qmat must be positive definite, amat must be irredicible
 *
 */
class HingeQuadraticOptimization {
  def quadProg(cvec: DenseVector[Double], qmat: DenseMatrix[Double], amat: DenseMatrix[Double]): DenseVector[Double] = {
    val n = cvec.size
    val m = amat.size / cvec.size
    val sm = 1e-10
    val umat = cholesky(qmat).t
    val uinv = inv(umat)
    val delta = -amat * uinv
    val y = uinv.t * cvec
    def worst(b2: DenseVector[Double]): Int = {
      if (max(b2) > sm)
        argmax(b2)
      else
        -1
    }
    @tailrec
    def improve(b2: DenseVector[Double], h: BitVector): DenseVector[Double] = {
      val xmat = delta(h.activeKeysIterator.toIndexedSeq, ::).toDenseMatrix
      val a = inv(xmat * xmat.t) * xmat * y
      val theta = xmat.t * a
      val b2 = delta * (y - theta)
      if (min(a) < (-sm)) {
        val avec = DenseVector.fill(m, 0.0)
        h.activeKeysIterator.zipWithIndex.foreach { case (r, i) => avec.update(r, a(i)) }
        val i = argmin(avec)
        h(i) = false
        improve(b2, h)
      } else {
        val w = worst(b2)
        if (w >= 0) {
          h(w) = true
          improve(b2, h)
        } else
          theta
      }
    }
    val w = worst(delta * y)
    val theta: DenseVector[Double] = if (w >= 0) {
      val h = BitVector(m)(w)
      improve(delta * y, h)
    } else
      DenseVector.zeros[Double](n)
    val bhat = uinv * (y - theta)
    bhat
  }
}