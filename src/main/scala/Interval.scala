import org.scalactic.Requirements._

case class Interval(index: Int, left: Double, right: Double) {
  require(left < right)
  require(index >= 0)
}