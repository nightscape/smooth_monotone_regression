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
    val Interval(index, left, right) = knots.intervalOf(x)
    val LastKnot = knots.length
    val NextToLastKnot = knots.length - 1
    val sigma = DenseVector.tabulate(knots.length + 1) {
      case (0) => 1.0
      case (NextToLastKnot) if index == 0 => 1 - scala.math.pow(x - right, 2) / (right - left) / (right - left)
      case (NextToLastKnot) if index >= 1 => 1.0
      case (LastKnot) if index == LastKnot - 2 => scala.math.pow(x - left, 2) / (right - left) / (right - left)
      case (LastKnot) if index >= LastKnot - 2 => 1.0
      case (j) if index < j - 1 => 0.0
      case (j) if index == j - 1 => scala.math.pow(x - left, 2) / (knots(j + 1) - left) / (right - left)
      case (j) if index == j => 1 - scala.math.pow(x - right, 2) / (right - left) / (right - knots(j - 1))
      case (j) if index >= j + 1 => 1.0
      case _ => throw new RuntimeException("Should not have gotten here")
    }
    sigma

  }
}
