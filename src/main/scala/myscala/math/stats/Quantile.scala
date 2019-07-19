package myscala.math.stats

/** Stores a single quantile of a Seq. Data type for computeQuantile function
  *
  * @param quantile define the quantile
  * @param value the value at that quantile
  * @param sampleSize the size of the data on which the quantile is calculated
  */
case class Quantile[T: Numeric](quantile: Double, value: T, sampleSize: Int)
