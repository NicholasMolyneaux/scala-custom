package myscala.math.stats

/** Stores a single quantile of a Seq. Data type for computeQuantile function
  *
  * @param quantile define the quantile
  * @param value the value at that quantile
  * @param sampleSize the size of the data on which the quantile is calculated
  */
case class Quantile[T: Numeric](quantile: Double, value: T, sampleSize: Int)


/** Storing multiple quantiles of the same Seq. Data type for computeQuantiles function
  *
  * @param quantiles defining the quantiles
  * @param values the values of the quantiles
  * @param sampleSize the size of the input Seq
  */
case class Quantiles[T: Numeric](quantiles: Seq[Double], values: Seq[T], sampleSize: Int)

/** Results from the statistics computation.
  *
  * @param size size of the data
  * @param mean mean of the data
  * @param variance variance of the data
  * @param median median of the data
  * @param min minimum value of the data
  * @param max maximum value of the data
  * @tparam T type of the data
  */
case class Stats[T: Numeric](size: Int, mean: Double, variance: Double, median: Double, min: T, max: T)
