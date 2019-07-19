package myscala.math.stats

/** Storing multiple quantiles of the same Seq. Data type for computeQuantiles function
  *
  * @param quantiles defining the quantiles
  * @param values the values of the quantiles
  * @param sampleSize the size of the input Seq
  */
case class Quantiles[T: Numeric](quantiles: Seq[Double], values: Seq[T], sampleSize: Int)
