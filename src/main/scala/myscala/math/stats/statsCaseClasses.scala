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
