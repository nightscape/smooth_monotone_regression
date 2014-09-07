import breeze.linalg._
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
    val m = amat.size / n
    var sm = 1e-10
    var h = Array.fill(m)(false)
    var obs = (0 until m - 1).toArray
    var check = 0
    val umat = cholesky(qmat).t
    val uinv = inv(umat)
    val delta = -amat * uinv
    val y = uinv.t * cvec
    var b2 = delta * y
    print(b2)
    var theta: DenseVector[Double] = null
    if (max(b2) > sm) {

      val i = argmax(b2)
      h(i) = true
    } else {
      check = 1
      theta = DenseVector.fill(n, 0.0)
    }
    while (check == 0) {
      val chosenRows = h.zipWithIndex.collect { case (true, i) => i }.toVector
      var xmat = DenseMatrix.tabulate(chosenRows.length, delta.cols) { (r, c) =>
        delta(chosenRows(r), c)
      }
      val a = inv(xmat * xmat.t) * xmat * y
      if (min(a) < (-sm)) {
        val avec = DenseVector.fill(m, 0.0)
        chosenRows.zipWithIndex.foreach { case (r, i) => avec.update(r, a(i)) }
        val i = argmin(avec)
        h(i) = false
        check = 0
      } else {
        check = 1
        theta = xmat.t * a
        b2 = delta * (y - theta)
        if (max(b2) > sm) {
          val i = argmax(b2)
          h(i) = true
          check = 0
        }
      }
    }
    val bhat = uinv * (y - theta)
    bhat
  }
}