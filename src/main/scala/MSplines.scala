import scala.math._
import org.scalactic.Requirements._
import scalax.chart.api._
object MSplines extends App {

  val knots = new UniformlyDistributedKnots(0.0, 1.0, 6)
  val order = 2
  def mSplines(i: Int) = new MSpline(knots)(order, i, _: Double)
  def iSplines(i: Int) = new ISpline(knots)(order, i, _: Double)
  val splines = (0 until knots.length - 2 + order).map(iSplines)
  val chart = new ChartWrapper(s"I-Splines of order $order", xlabel = "x", ylabel = "y", visible = true)
  splines.zipWithIndex.foreach {
    case (spline, index) =>
      val points = (0.0 to 1.0 by 1.0 / 512).map(x => (x, spline(x)))
      chart.addLines(points.toXYSeriesCollection(s"i = $index"))
  }

}