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
class MonotoneIncreasingTest extends PropSpec with PropertyChecks with Matchers with VectorMatchers with ROutputParser {

  def monotoneIncreasing(x: DenseVector[Double], t: DenseVector[Double]): DenseMatrix[Double] = {
    implicit val checkFileIterator = Source.fromFile("/tmp/monincr.txt").getLines()
    val n = x.length
    val k = t.length - 2
    val m = k + 3
    val sigma = DenseMatrix.tabulate(m, n) {
      case (r, j) =>
        (n * (1 + r)).toDouble
    }
    sigma(0, ::) := 1.0
    for (j <- 0 until (k - 1)) {
      //    i1=x<=t[j]
      //    sigma[j+1,i1] = 0
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(j)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(j + 1, i, 0))
      //    i2=x>t[j]&x<=t[j+1]
      //    sigma[j+1,i2] = (x[i2]-t[j])^2 / (t[j+2]-t[j]) / (t[j+1]-t[j])
      val i2 = x.mapPairs((ind, xv) => if (xv > t(j) && xv <= t(j + 1)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(j + 1, i, scala.math.pow(x(i) - t(j), 2) / (t(j + 2) - t(j)) / (t(j + 1) - t(j))))
      //    i3=x>t[j+1]&x<=t[j+2]
      //    sigma[j+1,i3] = 1-(x[i3]-t[j+2])^2/(t[j+2]-t[j+1])/(t[j+2]-t[j])
      val i3 = x.mapPairs((ind, xv) => if (xv > t(j + 1) && xv <= t(j + 2)) ind else -1).findAll(_ >= 0)
      i3.foreach(i => sigma.update(j + 1, i, 1 - scala.math.pow(x(i) - t(j + 2), 2) / (t(j + 2) - t(j + 1)) / (t(j + 2) - t(j))))
      //    i4=x>t[j+2]
      //    sigma[j+1,i4]=1
      val i4 = x.mapPairs((ind, xv) => if (xv > t(j + 2)) ind else -1).findAll(_ >= 0)
      i4.foreach(i => sigma.update(j + 1, i, 1.0))
    }

    {
      //i1=x<=t[k]
      //sigma[k+1,i1] = 0
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(k - 1)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(k, i, 0))
      //i2=x>t[k]&x<=t[k+1]
      //sigma[k+1,i2] = (x[i2]-t[k])^2 / (t[k+2]-t[k]) / (t[k+1]-t[k])
      val i2 = x.mapPairs((ind, xv) => if (xv > t(k - 1) && xv <= t(k)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(k, i, scala.math.pow(x(i) - t(k - 1), 2) / (t(k + 1) - t(k - 1)) / (t(k) - t(k - 1))))
      //i3=x>t[k+1]&x<=t[k+2]
      //sigma[k+1,i3] = 1- (x[i3]-t[k+2])^2/(t[k+2]-t[k+1])/(t[k+2]-t[k])
      val i3 = x.mapPairs((ind, xv) => if (xv > t(k) && xv <= t(k + 1)) ind else -1).findAll(_ >= 0)
      i3.foreach(i => sigma.update(k, i, 1 - scala.math.pow(x(i) - t(k + 1), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k - 1))))
      //i4=x>t[k+2]
      //sigma[k+1,i4]=1
      val i4 = x.mapPairs((ind, xv) => if (xv > t(k + 1)) ind else -1).findAll(_ >= 0)
      i4.foreach(i => sigma.update(k, i, 1.0))
    }
    {
      //i1=x<=t[2]
      //sigma[k+2,i1]=1-(t[2]-x[i1])^2/(t[2]-t[1])^2
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(1)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(k + 1, i, 1 - scala.math.pow(x(i) - t(1), 2) / (t(1) - t(0)) / (t(1) - t(0))))

      //i2=x>t[2]
      //sigma[k+2,i2]=1
      val i2 = x.mapPairs((ind, xv) => if (xv > t(1)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(k + 1, i, 1.0))
    }
    //
    //i1=x<=t[k+1]
    //sigma[k+3,i1]=0
    val i1 = x.mapPairs((ind, xv) => if (xv <= t(k)) ind else -1).findAll(_ >= 0)
    i1.foreach(i => sigma.update(k + 2, i, 0))

    //i2=x>t[k+1]&x<=t[k+2]
    //sigma[k+3,i2]=(x[i2]-t[k+1])^2/(t[k+2]-t[k+1])^2
    val i2 = x.mapPairs((ind, xv) => if (xv > t(k) && xv <= t(k + 1)) ind else -1).findAll(_ >= 0)
    i2.foreach(i => sigma.update(k + 2, i, scala.math.pow(x(i) - t(k), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k))))
    //i3=x>t[k+2]
    //sigma[k+3,i3]=1
    val i3 = x.mapPairs((ind, xv) => if (xv > t(k + 1)) ind else -1).findAll(_ >= 0)
    i3.foreach(i => sigma.update(k + 2, i, 1.0))

    for (i <- 1 until m) {
      val sigs = sigma(i, ::)
      val ms = breeze.linalg.sum(sigs.t) / sigs.t.size
      sigs := sigs - ms
    }
    sigma
  }

  property("creates same results as R code") {
    val x = DenseVector[Double](1, 2.4, 2.5, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.1, 4.6, 4.7, 4.8, 5.3, 5.4, 5.8, 5.9, 6, 6.1, 7, 7.1, 7.2, 7.3, 7.9, 8.3, 19, 20)
    val t = DenseVector[Double](1, 3, 3.5, 4.1, 5.8, 7.2, 20)
    val sigma = new DenseMatrix[Double](8, 33, Array(1, -0.9021818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.964697, -0.05637244, 1, -0.5101818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.05469697, -0.05637244, 1, -0.4521818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, -0.02719697, -0.05637244, 1, -0.2541818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.02530303, -0.05637244, 1, -0.1801818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.03280303, -0.05637244, 1, -0.1021818, -0.6721763, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, -0.03018182, -0.6539945, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.02581818, -0.599449, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.06581818, -0.5085399, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.08981818, -0.3812672, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, -0.2176309, -0.4981787, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, -0.05096419, -0.4909323, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.08539945, -0.4691932, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.1914601, -0.4329613, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.2672176, -0.3822367, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, -0.2373092, -0.3436219, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.1335348, -0.2961836, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.1923584, -0.2753107, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.2460668, -0.2506428, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.4378827, -0.07037712, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.4609006, -0.02293879, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.2047652, -0.08519845, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.2669772, -0.08469544, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.3245809, -0.08318638, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.3775763, -0.08067129, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6471615, -0.01276385, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6540739, -0.0001883936, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.0133931, 0.03530303, -0.05637244, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.02742258, 0.03530303, -0.05631141, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.1092888, 0.03530303, -0.05338172, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.1616655, 0.03530303, -0.04898719, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.9092998, 0.03530303, 0.7934811, 1, 0.09781818, 0.3278237, 0.5018213, 0.6563781, 0.9148015, 0.03530303, 0.9436276))

    val result = monotoneIncreasing(x, t)
    result should beMatrixSimilarTo(sigma, allowedDeviation = 1.0E-4)
  }
}