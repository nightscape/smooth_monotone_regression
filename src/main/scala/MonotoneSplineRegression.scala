import breeze.linalg._

import com.joptimizer.functions.PDQuadraticMultivariateRealFunction
import com.joptimizer.functions.ConvexMultivariateRealFunction
import com.joptimizer.functions.LinearMultivariateRealFunction
import com.joptimizer.optimizers.OptimizationRequest
import com.joptimizer.optimizers.JOptimizer

class MonotoneSplineRegression(knots: Knots) {
  def toArrayMatrix(matrix: DenseMatrix[Double]): Array[Array[Double]] =
    Array.tabulate(matrix.rows, matrix.cols)(matrix.apply)
  val order = 2
  def iSplines(i: Int) = new NumericalISpline(knots)(order, i, _: Double)
  val splines = (0 until knots.length - 2 + order).map(iSplines)
  def learn(xs: Array[Double], ys: Array[Double]): Double => Double = {
    val regressionMatrix = DenseMatrix.tabulate(xs.size, splines.size) { (ix, is) =>
      val x = xs(ix)
      val spline = splines(is)
      spline(x)
    }
    val errorMatrix = DenseMatrix.eye[Double](xs.size)
    val constantMatrix = DenseMatrix.ones[Double](xs.size, 1)
    val equationMatrix = DenseMatrix.horzcat(regressionMatrix, errorMatrix, constantMatrix)
    val equationArrMatrix = toArrayMatrix(equationMatrix)
    val restrictionMatrix = DenseMatrix.horzcat(-DenseMatrix.eye[Double](splines.size), DenseMatrix.zeros[Double](splines.size, xs.size + 1))
    val minimizationMatrix = DenseMatrix.vertcat(DenseMatrix.zeros[Double](splines.size + 1, splines.size + xs.size + 1), DenseMatrix.horzcat(regressionMatrix * 0.0, errorMatrix, constantMatrix * 0.0))
    println(s"x dims ${xs.size}")
    println(s"splines dims ${splines.size}")
    println(s"Minimiz dims ${minimizationMatrix.rows} ${minimizationMatrix.cols}")
    println(s"Equatio dims ${equationMatrix.rows} ${equationMatrix.cols}")
    println(s"Restric dims ${restrictionMatrix.rows} ${restrictionMatrix.cols}")
    import com.softwaremill.debug.DebugConsole._
    println("Matrices")
    println("Mimimization")
    println(minimizationMatrix)
    println("Equation")
    println(equationMatrix)
    println("Restriction")
    println(restrictionMatrix)
    val objectiveFunction = new PDQuadraticMultivariateRealFunction(toArrayMatrix(minimizationMatrix), null, 0)
    val inequalities = (0 until restrictionMatrix.rows).toArray.map { r =>
      val restrictionRow = (0 until restrictionMatrix.cols).toArray.map { c => restrictionMatrix(r, c) }
      new LinearMultivariateRealFunction(restrictionRow, 0):ConvexMultivariateRealFunction
    }
    val or = new OptimizationRequest()
    or.setF0(objectiveFunction)
    //or.setInitialPoint(Array.fill[Double](splines.size + xs.size + 1)(0.5))
    or.setA(equationArrMatrix)
    or.setB(ys)
    or.setFi(inequalities)
    or.setToleranceFeas(1.0E-12)
    or.setTolerance(1.0E-12)
    val opt = new JOptimizer()
    opt.setOptimizationRequest(or)
    val returnCode = opt.optimize()
    val sol = opt.getOptimizationResponse().getSolution()
    println(sol.toList)
    val f = { x: Double => 1 / x }
    f
  }
}