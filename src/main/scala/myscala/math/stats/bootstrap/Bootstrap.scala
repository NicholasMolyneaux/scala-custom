package myscala.math.stats.bootstrap

/** Container for the results of a bootstrap. The function is also stored so it is easier to find
  * what parameter has been computed.
  *
  * @param MSE mean square error of the parameter
  * @param parameter the value of the parameter on the data
  * @param parameterFunction function used to compute the statistic
  * @tparam T type of the data
  */
case class Bootstrap[T: Numeric](MSE: Double, parameter: Double, parameterFunction: Seq[T] => Double) {

  override def toString: String = "Bootstrap results. Parameter: " + this.parameter + ", MSE: " + MSE

  def toCSV: String = this.parameter + "," + this.MSE

  def toTuple: (Double, Double) = (this.parameter, this.MSE)
}
