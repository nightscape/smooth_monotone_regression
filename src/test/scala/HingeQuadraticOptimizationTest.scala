import breeze.linalg._
import org.scalatest.Matchers
import java.io.FileWriter
import java.text.MessageFormat
import java.util.Locale
import org.scalatest.matchers._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class HingeQuadraticOptimizationTest extends PropSpec with PropertyChecks with Matchers with VectorMatchers with ROutputParser {

  def quadProg(cvec: DenseVector[Double], qmat: DenseMatrix[Double], amat: DenseMatrix[Double]): DenseVector[Double] = {
    implicit val checkFileIterator = Source.fromFile("/tmp/quadprog.txt").getLines()
    checkMatrix("cvec", cvec.toDenseMatrix.t)
    checkMatrix("qmat", qmat)
    checkMatrix("amat", amat)
    val n = cvec.size
    val m = amat.size / n
    var sm = 1e-10
    var h = Array.fill(m)(false)
    var obs = (0 until m - 1).toArray
    var check = 0
    val umat = cholesky(qmat).t
    checkMatrix("umat", umat)
    val uinv = inv(umat)
    checkMatrix("uinv", uinv)
    val delta = -amat * uinv
    checkMatrix("delta", delta.toDenseMatrix)
    val y = uinv.t * cvec
    checkMatrix("y", y.toDenseMatrix.t)
    var b2 = delta * y
    checkMatrix("b2", b2.toDenseMatrix.t)
    print(b2)
    var theta: DenseVector[Double] = null
    if (max(b2) > sm) {

      val i = b2.argmax
      checkMatrix("i", new DenseMatrix[Double](1, 1, Array(i.toDouble)))
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
      checkMatrix("xmat", xmat)
      val a = inv(xmat * xmat.t) * xmat * y
      println(s"a is\n$a")
      checkMatrix("a", a.toDenseMatrix)
      println("Checked a OK")
      println(s"Min of a is ${min(a)}")
      if (min(a) < (-sm)) {
        println("Inside if")
        val avec = DenseVector.fill(m, 0.0)
        println(s"Before updating avec $avec")
        chosenRows.zipWithIndex.foreach { case (r, i) => avec.update(r, a(i)) }
        println(s"After updating avec $avec")
        checkMatrix("avec", avec.toDenseMatrix * 2.0)
        val i = argmin(avec)
        h(i) = false
        check = 0
      } else {
        check = 1
        theta = xmat.t * a
        checkMatrix("theta", theta.toDenseMatrix)
        b2 = delta * (y - theta)
        checkMatrix("b2", b2.toDenseMatrix)
        if (max(b2) > sm) {
          val i = b2.argmax
          checkMatrix("i", new DenseMatrix[Double](1, 1, Array(i.toDouble)))
          h(i) = true
          check = 0
        }
      }
    }
    val bhat = uinv * (y - theta)
    checkMatrix("bhat", bhat.toDenseMatrix)
    bhat
  }
  property("creates same results as R code") {
    val qmat = DenseMatrix.create(8, 8, Array(33, 6.661338e-16, -4.440892e-16, -1.776357e-15, 8.881784e-16, 4.440892e-16, -8.881784e-16, 7.771561e-16, 6.661338e-16, 1.620251, 2.154658, 1.608121, 1.109212, 0.2750206, 0.9793518, 0.1819702, -4.440892e-16, 2.154658, 6.001698, 5.363696, 3.717364, 0.9216924, 0.7830854, 0.6098473, -1.776357e-15, 1.608121, 5.363696, 7.122483, 5.59743, 1.410895, 0.5803782, 0.9335334, 8.881784e-16, 1.109212, 3.717364, 5.59743, 5.913784, 1.84245, 0.4003195, 1.221054, 4.440892e-16, 0.2750206, 0.9216924, 1.410895, 1.84245, 1.883156, 0.0992562, 1.689096, -8.881784e-16, 0.9793518, 0.7830854, 0.5803782, 0.4003195, 0.0992562, 0.9709845, 0.06567389, 7.771561e-16, 0.1819702, 0.6098473, 0.9335334, 1.221054, 1.689096, 0.06567389, 1.617445))
    val amat = DenseMatrix.create(8, 8, Array[Double](0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1))
    val cvec = DenseVector(14.38177, 1.010953, 2.554774, 2.984773, 3.118192, 1.27234, 0.4823596, 0.912791)
    val bhat = DenseVector(0.4358112, 0.1342831, 0.1105716, 2.349282e-16, 0.3448744, 0.2590386, 0.1034939, -2.499804e-16)
    val result = quadProg(cvec, qmat, amat)
    result should beSimilarTo(bhat, allowedDeviation = 1.0E-4)
  }
}