import breeze.linalg._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait VectorMatchers {

  class VectorsSimilar(right: DenseVector[Double], allowedDeviation: Double = 0.0, norm: Double = 2.0) extends Matcher[DenseVector[Double]] {

    def apply(left: DenseVector[Double]) = {
      val deviation = (left - right).norm(norm)
      val failureMessageSuffix = s"vector $left deviates by $deviation from $right, expected deviation <= $allowedDeviation (norm = $norm)"

      val negatedFailureMessageSuffix = s"vector $left deviates by $deviation from $right, expected deviation > $allowedDeviation (norm = $norm)"

      MatchResult(
        deviation <= allowedDeviation,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix)
    }
  }

  def beSimilarTo(right: DenseVector[Double], allowedDeviation: Double = 0.0, norm: Double = 2.0) = new VectorsSimilar(right, allowedDeviation, norm)

  class VectorElementsSmaller(right: DenseVector[Double]) extends Matcher[DenseVector[Double]] {
    def apply(left: DenseVector[Double]) = {
      val notSmaller = left :> right
      val notSmallerIndeces = notSmaller.findAll(a => a)
      val notSmallerPairs = notSmallerIndeces.map(i => s"$i: ${left(i)} > ${right(i)}")
      val failureMessageSuffix = s"vector $left is bigger than $right at indeces $notSmallerIndeces:\n${notSmallerPairs.mkString("\n")}"

      val negatedFailureMessageSuffix = s"not $failureMessageSuffix"

      MatchResult(
        !any(notSmaller),
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix)
    }
  }

  def haveSmallerElementsThan(right: DenseVector[Double]): Matcher[DenseVector[Double]] = new VectorElementsSmaller(right)
  class MatrixSimilar(right: DenseMatrix[Double], allowedDeviation: Double = 0.0, norm: Double = 2.0) extends Matcher[DenseMatrix[Double]] {

    def apply(left: DenseMatrix[Double]) = {
      import breeze.linalg.norm._
      val difference = left - right
      val deviation = breeze.linalg.max(breeze.numerics.abs(difference))
      val failureMessageSuffix = s"matrix\n$left deviates by $deviation from\n$right, expected deviation <= $allowedDeviation (norm = $norm). Difference:\n${left - right}"

      val negatedFailureMessageSuffix = s"matrix $left deviates by $deviation from $right, expected deviation > $allowedDeviation (norm = $norm)"

      MatchResult(
        deviation <= allowedDeviation,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix)
    }
  }

  def beMatrixSimilarTo(right: DenseMatrix[Double], allowedDeviation: Double = 0.0, norm: Double = 2.0) = new MatrixSimilar(right, allowedDeviation, norm)

}

object VectorMatchers extends VectorMatchers
