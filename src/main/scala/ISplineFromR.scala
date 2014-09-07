import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

object ISplineFromR {
  def monotoneIncreasing(x: DenseVector[Double], t: DenseVector[Double]): DenseMatrix[Double] = {
    val n = x.length
    val k = t.length - 2
    val m = k + 3
    val sigma = DenseMatrix.tabulate(m, n) {
      case (r, j) =>
        (n * (1 + r)).toDouble
    }
    sigma(0, ::) := 1.0
    for (j <- 0 until (k - 1)) {
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(j)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(j + 1, i, 0))
      val i2 = x.mapPairs((ind, xv) => if (xv > t(j) && xv <= t(j + 1)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(j + 1, i, scala.math.pow(x(i) - t(j), 2) / (t(j + 2) - t(j)) / (t(j + 1) - t(j))))
      val i3 = x.mapPairs((ind, xv) => if (xv > t(j + 1) && xv <= t(j + 2)) ind else -1).findAll(_ >= 0)
      i3.foreach(i => sigma.update(j + 1, i, 1 - scala.math.pow(x(i) - t(j + 2), 2) / (t(j + 2) - t(j + 1)) / (t(j + 2) - t(j))))
      val i4 = x.mapPairs((ind, xv) => if (xv > t(j + 2)) ind else -1).findAll(_ >= 0)
      i4.foreach(i => sigma.update(j + 1, i, 1.0))
    }

    {
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(k - 1)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(k, i, 0))
      val i2 = x.mapPairs((ind, xv) => if (xv > t(k - 1) && xv <= t(k)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(k, i, scala.math.pow(x(i) - t(k - 1), 2) / (t(k + 1) - t(k - 1)) / (t(k) - t(k - 1))))
      val i3 = x.mapPairs((ind, xv) => if (xv > t(k) && xv <= t(k + 1)) ind else -1).findAll(_ >= 0)
      i3.foreach(i => sigma.update(k, i, 1 - scala.math.pow(x(i) - t(k + 1), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k - 1))))
      val i4 = x.mapPairs((ind, xv) => if (xv > t(k + 1)) ind else -1).findAll(_ >= 0)
      i4.foreach(i => sigma.update(k, i, 1.0))
    }
    {
      val i1 = x.mapPairs((ind, xv) => if (xv <= t(1)) ind else -1).findAll(_ >= 0)
      i1.foreach(i => sigma.update(k + 1, i, 1 - scala.math.pow(x(i) - t(1), 2) / (t(1) - t(0)) / (t(1) - t(0))))

      val i2 = x.mapPairs((ind, xv) => if (xv > t(1)) ind else -1).findAll(_ >= 0)
      i2.foreach(i => sigma.update(k + 1, i, 1.0))
    }
    val i1 = x.mapPairs((ind, xv) => if (xv <= t(k)) ind else -1).findAll(_ >= 0)
    i1.foreach(i => sigma.update(k + 2, i, 0))

    val i2 = x.mapPairs((ind, xv) => if (xv > t(k) && xv <= t(k + 1)) ind else -1).findAll(_ >= 0)
    i2.foreach(i => sigma.update(k + 2, i, scala.math.pow(x(i) - t(k), 2) / (t(k + 1) - t(k)) / (t(k + 1) - t(k))))
    val i3 = x.mapPairs((ind, xv) => if (xv > t(k + 1)) ind else -1).findAll(_ >= 0)
    i3.foreach(i => sigma.update(k + 2, i, 1.0))

    for (i <- 1 until m) {
      val sigs = sigma(i, ::)
      val ms = breeze.linalg.sum(sigs.t) / sigs.t.size
      sigs := sigs - ms
    }
    sigma
  }
}