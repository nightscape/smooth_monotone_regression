import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import org.scalatest.Matchers

trait ROutputParser extends Matchers with VectorMatchers {
  object IntString {
    def unapply(s: String): Option[Option[Int]] = if (s == null) Some(None) else try {
      Some(Some(s.toInt))
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }
  object NumbersString {
    def unapply(s: String): Option[Array[Double]] = try {
      val strings = s.split(", ")
      Some(strings.map(_.toDouble))
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }
  object Optional {
    def unapply[T](a: T) = if (null == a) Some(None) else Some(Some(a))
  }
  import scala.io.Source

  def checkMatrix(varName: String, matrix: DenseMatrix[Double])(implicit checkFileIterator: Iterator[String]) {
    println(s"Checking $varName")
    var currentLine = ""
    do {
      currentLine = checkFileIterator.next()
    } while (!currentLine.startsWith(varName))
    val matrixExpression = """(\w+) \((?: (\d+))?(?: (\d+))? \) (.*)""".r
    val matrixExpression(theVar, IntString(rowsOption), IntString(columnsOption), NumbersString(numbers)) = currentLine
    val rows = rowsOption.getOrElse(1)
    val columns = columnsOption.getOrElse(numbers.size)
    if (varName == "a")
      println(s"rows is $rows, cols is $columns, numbers are ${numbers.toVector}")
    if (columns == 1) {
      val expected = DenseVector[Double](numbers)
      matrix.toDenseVector should beSimilarTo(expected, allowedDeviation = 1.0E-4)
      println("OK")
    } else {
      val expected = new DenseMatrix[Double](rows, columns, numbers)
      matrix should beMatrixSimilarTo(expected, allowedDeviation = 1.0E-4)
      println("OK")
    }

  }

}