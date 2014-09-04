
import breeze.plot._
import breeze.linalg._
import java.awt.EventQueue
import org.jfree.ui.RefineryUtilities
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.Paint
import java.awt.Color
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeriesCollection
import scalax.chart._
import scalax.chart.api._
import org.jfree.chart.labels.XYItemLabelGenerator
object Plotter {
  implicit def func2TooltipGenerator(generator: (XYDataset, Int, Int) => String) = new org.jfree.chart.labels.XYToolTipGenerator {
    override def generateToolTip(dataset: XYDataset, series: Int, item: Int): String =
      generator(dataset, series, item)
  }
  implicit def func2LabelGenerator(generator: (XYDataset, Int, Int) => String) = new org.jfree.chart.labels.XYItemLabelGenerator {
    override def generateLabel(dataset: XYDataset, series: Int, item: Int): String =
      generator(dataset, series, item)
  }
}

/**
 * A custom renderer that returns a different color for each item in a single series.
 */
class CustomRenderer(rgbFunction: (Int, Int) => (Double, Double, Double)) extends XYLineAndShapeRenderer {
  import Plotter._
  this.setBaseToolTipGenerator({ (d: XYDataset, r: Int, c: Int) => s"$d $r $c" })
  this.setSeriesLinesVisible(0, false)
  this.setSeriesShapesVisible(0, true)
  this.setSeriesShape(0, Plot.plus)
  override def getItemPaint(row: Int, column: Int): Paint = {
    val (r, g, b) = rgbFunction(row, column)
    new Color(r.toFloat, g.toFloat, b.toFloat)
  }
}

class WeightedRenderer(renderer: XYItemRenderer, weight: (Int, Int) => Double) extends XYLineAndShapeRenderer {
  this.setSeriesLinesVisible(0, false)
  this.setSeriesShapesVisible(0, true)
  this.setSeriesShape(0, null)
  override def getItemPaint(row: Int, column: Int): Paint = {
    val rgb = renderer.getItemPaint(row, column).asInstanceOf[Color]
    new Color(rgb.getRed(), rgb.getGreen(), rgb.getBlue(), (scala.math.min(1.0, scala.math.max(0.0, weight(row, column))) * 255).toInt)
  }
}

class ChartWrapper(val name: String, xlabel: String = "x", ylabel: String = "y", visible: Boolean = true) {
  val f = Figure()
  f.visible = visible
  val figurePlot = f.subplot(0)
  figurePlot.title = name
  figurePlot.legend = true
  figurePlot.xlabel = xlabel
  figurePlot.ylabel = ylabel
  def plot = figurePlot.plot
  def +=(data: XYSeriesCollection, renderer: XYItemRenderer, tooltipGenerator: Option[XYToolTipGenerator] = None, labelGenerator: Option[XYItemLabelGenerator] = None) = {
    tooltipGenerator.map(renderer.setBaseToolTipGenerator)
    labelGenerator.map(renderer.setItemLabelGenerator)
    renderer.setItemLabelsVisible(true)

    val currentDataset = plot.getDatasetCount() + 1
    plot.setDataset(currentDataset, data)
    plot.setRenderer(currentDataset, renderer)
    renderer
  }
  def addPoints(data: XYSeriesCollection, tooltipGenerator: Option[XYToolTipGenerator] = None, labelGenerator: Option[XYItemLabelGenerator] = None, weighting: Option[(Int, Int) => Double] = None) = {
    this += (data, weightedRenderer(new XYLineAndShapeRenderer(false, true), weighting), tooltipGenerator, labelGenerator)
  }

  def weightedRenderer(renderer: XYItemRenderer, weighting: Option[(Int, Int) => Double] = None) =
    weighting.map(w => new WeightedRenderer(renderer, w)).getOrElse(renderer)

  def addLines(data: XYSeriesCollection, tooltipGenerator: Option[XYToolTipGenerator] = None) = {
    this += (data, new XYLineAndShapeRenderer(true, false), tooltipGenerator)
  }
  def addLinesAndPoints(lines: XYSeriesCollection, points: XYSeriesCollection, tooltipGenerator: Option[XYToolTipGenerator] = None, labelGenerator: Option[XYItemLabelGenerator] = None, weighting: Option[(Int, Int) => Double] = None) {
    val lineRenderer = addLines(lines)
    this += (points, weightedRenderer(lineRenderer, weighting), tooltipGenerator, labelGenerator)
  }
  def save(filePath:String) {
    f.saveas(s"$filePath.png")
  }
}
