import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

class ISplineFromR(knots: Knots) extends (Double => DenseVector[Double]){
  def monotoneIncreasing(x: DenseVector[Double], normalizeCoefficients: Boolean = false): DenseMatrix[Double] = {
    val n = x.length
    val k = knots.length - 2
    val m = k + 3
    val sigma = DenseMatrix.horzcat(x.toArray.map(apply).map(_.toDenseMatrix.t): _*)
    if (normalizeCoefficients) {
      for (i <- 1 until m) {
        val sigs = sigma(i, ::)
        val ms = breeze.linalg.sum(sigs.t) / sigs.t.size
        sigs := sigs - ms
      }
    }
    sigma
  }
  def apply(x: Double): DenseVector[Double] = {
    val k = knots.length - 2
    val m = k + 3
    val Interval(index, left, right) = knots.intervalOf(x)
    val sigma = DenseVector.tabulate(m) {
      case (0) => 1.0
      case (j) if j == k + 1 && index == 0 => 1 - scala.math.pow(x - right, 2) / (right - left) / (right - left)
      case (j) if j == k + 1 && index >= 1 => 1.0
      case (j) if j == k + 1 && index == k + 2 => 1.0
      case (j) if j == k + 2 && index < k => 0.0
      case (j) if j == k + 2 && index == k => scala.math.pow(x - left, 2) / (right - left) / (right - left)
      case (j) if j == k + 2 && index >= k => 1.0
      case (j) if j == k && index < k - 1 => 0.0
      case (j) if j == k && index == k - 1 => scala.math.pow(x - left, 2) / (knots(k + 1) - left) / (right - left)
      case (j) if j == k && index == k => 1 - scala.math.pow(x - right, 2) / (right - left) / (right - knots(k - 1))
      case (j) if index < j - 1 => 0.0
      case (j) if index == j - 1 => scala.math.pow(x - left, 2) / (knots(j + 1) - left) / (right - left)
      case (j) if index == j => 1 - scala.math.pow(x - right, 2) / (right - left) / (right - knots(j - 1))
      case (j) if index >= j + 1 => 1.0
      case _ => throw new RuntimeException("Should not have gotten here")
    }
    sigma

  }
}
