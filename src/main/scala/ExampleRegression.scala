import scalax.chart.api._
import breeze.linalg._

object ExampleRegression extends App {

  val x = DenseVector(1.0, 2.4, 2.5, 2.8, 2.9, 3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.1, 4.6, 4.7, 4.8, 5.3, 5.4, 5.8, 5.9, 6.0, 6.1, 7.0, 7.1, 7.2, 7.3, 7.9, 8.3, 19.0, 20.0)
  val y = DenseVector(1.0, 0.8650853889943074, 0.8253169651841417, 0.828916531458446, 0.7641408751334045, 0.7811688311688312, 0.8132763975155279, 0.7882749894559258, 0.7814696485623003, 0.7440363498674745, 0.7047361299052775, 0.7020771513353116, 0.4127194435243458, 0.7023848415550473, 0.6080500195388824, 0.7120398773006135, 0.6062407132243685, 0.5587112171837709, 0.6064116379310345, 0.6113342257920571, 0.5585951940850278, 0.6108081308874567, 0.5648216482164822, 0.5658691265494379, 0.5274445656125045, 0.24324324324324326, 0.1951219512195122, 0.16129032258064516, 0.21052631578947367, 0.29411764705882354, 0.12, 0.1, 0.05)
  val knots = DenseVector[Double](1, 3, 3.5, 4.1, 5.8, 7.2, 20)
  val monotoneSplines = new MonotoneSplineRegressionFromR(new ArbitrarilyDistributedKnots(knots.toArray), -1)
  val predictor = monotoneSplines.learn(x, y)
  val chart = new ChartWrapper(s"I-Spline Regression", xlabel = "x", ylabel = "y", visible = true)
  chart.addPoints(x.data.zip(y.data).toList.toXYSeriesCollection(s"Observations"))
  chart.addLines((min(min(x), min(knots)) until max(max(x), max(knots)) by 0.1).map(p => (p, predictor(p))).toXYSeriesCollection("Regression"))
}