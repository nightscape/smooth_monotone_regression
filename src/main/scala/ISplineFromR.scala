import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

class ISplineFromR(t: DenseVector[Double]) {
  def monotoneIncreasing(x: DenseVector[Double]): DenseMatrix[Double] = {
    val n = x.length
    val k = t.length - 2
    val m = k + 3
    val sigma = DenseMatrix.tabulate(m, n) {
      case (0, _) => 1.0
      case (j, r) if j == k + 1 && x(r) <= t(1) => 1 - scala.math.pow(x(r) - t(1), 2) / (t(1) - t(0)) / (t(1) - t(0))
      case (j, r) if j == k + 1 && x(r) > t(1) => 1.0
      case (j, r) if j == k + 1 && x(r) > t(k + 2) => 1.0
      case (j, r) if j == k + 2 && x(r) <= t(k) => 0.0
      case (j, r) if j == k + 2 && x(r) > t(k) && x(r) <= t(k + 1) => scala.math.pow(x(r) - t(k), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k))
      case (j, r) if j == k + 2 && x(r) > t(k + 1) => 1.0
      case (j, r) if j == k && x(r) <= t(k - 1) => 0.0
      case (j, r) if j == k && x(r) > t(k - 1) && x(r) <= t(k) => scala.math.pow(x(r) - t(k - 1), 2) / (t(k + 1) - t(k - 1)) / (t(k) - t(k - 1))
      case (j, r) if j == k && x(r) > t(k) && x(r) <= t(k + 1) => 1 - scala.math.pow(x(r) - t(k + 1), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k - 1))
      case (j, r) if x(r) <= t(j - 1) => 0.0
      case (j, r) if x(r) > t(j - 1) && x(r) <= t(j) => scala.math.pow(x(r) - t(j - 1), 2) / (t(j + 1) - t(j - 1)) / (t(j) - t(j - 1))
      case (j, r) if x(r) > t(j) && x(r) <= t(j + 1) => 1 - scala.math.pow(x(r) - t(j + 1), 2) / (t(j + 1) - t(j)) / (t(j + 1) - t(j - 1))
      case (j, r) if x(r) > t(j+1) => 1.0
      case _ => throw new RuntimeException("Should not have gotten here")
    }

    for (i <- 1 until m) {
      val sigs = sigma(i, ::)
      val ms = breeze.linalg.sum(sigs.t) / sigs.t.size
      sigs := sigs - ms
    }
    sigma
  }
}