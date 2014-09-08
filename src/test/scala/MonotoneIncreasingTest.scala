import breeze.linalg._
import org.scalatest.Matchers
import org.scalatest.matchers._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MonotoneIncreasingTest extends PropSpec with PropertyChecks with Matchers with VectorMatchers with ROutputParser {
  property("creates same results as R code") {
    val x = DenseVector[Double](1, 2.4, 2.5, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.1, 4.6, 4.7, 4.8, 5.3, 5.4, 5.8, 5.9, 6, 6.1, 7, 7.1, 7.2, 7.3, 7.9, 8.3, 19, 20)
    val t = DenseVector[Double](1, 3, 3.5, 4.1, 5.8, 7.2, 20)
    val sigma = new DenseMatrix[Double](8, 33, Array(1, -0.9021818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.964697, -0.05637244, 1, -0.5101818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.05469697, -0.05637244, 1, -0.4521818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.02719697, -0.05637244, 1, -0.2541818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.02530303, -0.05637244, 1, -0.1801818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.03280303, -0.05637244, 1, -0.1021818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, -0.03018182, -0.6539945, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.02581818, -0.599449, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.06581818, -0.5085399, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.08981818, -0.3812672, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, -0.2176309, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, -0.05096419, -0.4909323, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.08539945, -0.4691932, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.1914601, -0.4329613, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.2672176, -0.3822367, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, -0.2373092, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.1335348, -0.2961836, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.1923584, -0.2753107, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.2460668, -0.2506428, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.4378827, -0.07037712, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.4609006, -0.02293879, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.2047652, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.2669772, -0.08469544, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.3245809, -0.08318638, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.3775763, -0.08067129, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6471615, -0.01276385, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6540739, -0.0001883936, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.0133931, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.02742258, 0.03530303, -0.05631141, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.1092888, 0.03530303, -0.05338172, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.1616655, 0.03530303, -0.04898719, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.9092998, 0.03530303, 0.7934811, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.9148015, 0.03530303, 0.9436276))

    val result = new ISplineFromR(new ArbitrarilyDistributedKnots(t.toArray)).monotoneIncreasing(x, normalizeCoefficients = true)
    result should beMatrixSimilarTo(sigma, allowedDeviation = 1.0E-4)
  }
}