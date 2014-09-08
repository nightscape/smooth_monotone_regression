import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

class ISplineFromR(t: DenseVector[Double]) {
  val knots = new ArbitrarilyDistributedKnots(t.toArray)
  def monotoneIncreasing(x: DenseVector[Double], normalizeCoefficients: Boolean = false): DenseMatrix[Double] = {
    val n = x.length
    val k = t.length - 2
    val m = k + 3
    val sigma = DenseMatrix.horzcat(x.toArray.map(monIncr).map(_.toDenseMatrix.t): _*)
    if (normalizeCoefficients) {
      for (i <- 1 until m) {
        val sigs = sigma(i, ::)
        val ms = breeze.linalg.sum(sigs.t) / sigs.t.size
        sigs := sigs - ms
      }
    }
    sigma
  }
  def monIncr(x: Double): DenseVector[Double] = {
    val k = t.length - 2
    val m = k + 3
    val Interval(index, left, right) = knots.intervalOf(x)
    val sigma = DenseVector.tabulate(m) {
      case (0) => 1.0
      case (j) if j == k + 1 && index == 0 => 1 - scala.math.pow(x - t(1), 2) / (t(1) - t(0)) / (t(1) - t(0))
      case (j) if j == k + 1 && index >= 1 => 1.0
      case (j) if j == k + 1 && index == k + 2 => 1.0
      case (j) if j == k + 2 && index < k => 0.0
      case (j) if j == k + 2 && index == k => scala.math.pow(x - t(k), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k))
      case (j) if j == k + 2 && index >= k => 1.0
      case (j) if j == k && index < k - 1 => 0.0
      case (j) if j == k && index == k - 1 => scala.math.pow(x - t(k - 1), 2) / (t(k + 1) - t(k - 1)) / (t(k) - t(k - 1))
      case (j) if j == k && index == k => 1 - scala.math.pow(x - t(k + 1), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k - 1))
      case (j) if index < j - 1 => 0.0
      case (j) if index == j - 1 => scala.math.pow(x - t(j - 1), 2) / (t(j + 1) - t(j - 1)) / (t(j) - t(j - 1))
      case (j) if index == j => 1 - scala.math.pow(x - t(j + 1), 2) / (t(j + 1) - t(j)) / (t(j + 1) - t(j - 1))
      case (j) if index >= j + 1 => 1.0
      case _ => throw new RuntimeException("Should not have gotten here")
    }
    sigma

  }
}
