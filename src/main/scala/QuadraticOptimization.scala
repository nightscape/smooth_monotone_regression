

import com.joptimizer.functions.PDQuadraticMultivariateRealFunction
import com.joptimizer.functions.ConvexMultivariateRealFunction
import com.joptimizer.functions.LinearMultivariateRealFunction
import com.joptimizer.optimizers.OptimizationRequest
import com.joptimizer.optimizers.JOptimizer

object QuadraticOptimization extends App {
  val P = Array(Array(1.0, 0.4), Array(0.4, 1.0))
  val objectiveFunction = new PDQuadraticMultivariateRealFunction(P, null, 0)
  val A = Array(Array(1.0, 1.0))
  val b = Array(1.0)
  val inequalities = Array.ofDim[ConvexMultivariateRealFunction](2)
  inequalities(0) = new LinearMultivariateRealFunction(Array(-1, 0), 0)
  inequalities(1) = new LinearMultivariateRealFunction(Array(0, -1), 0)
  val or = new OptimizationRequest()
  or.setF0(objectiveFunction)
  or.setInitialPoint(Array(0.1, 0.9))
  or.setA(A)
  or.setB(b)
  or.setToleranceFeas(1.0E-12)
  or.setTolerance(1.0E-12)
  val opt = new JOptimizer()
  opt.setOptimizationRequest(or)
  val returnCode = opt.optimize()
  val sol = opt.getOptimizationResponse().getSolution()
  println(sol.toList)
}