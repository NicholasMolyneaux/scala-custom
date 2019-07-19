package myscala.math.stats

/**
  * Container for the results of computing the statistics of an sequence.
  * @param size number of values in the sample
  * @param mean mean of the sample
  * @param median median of the sample
  * @param min minimum
  * @param max maximum
  * @param variance variance
  * @tparam T numeric type
  */
case class Statistics[T: Numeric](size: Int, mean: Double, median: Double, min: T, max: T, variance: Double) {

  val CSVColumnNames: String = "size,mean,median,min,max,variance"

  def toCSV: String =
    this.size + "," + this.mean + "," + this.median + "," + this.min + "," + this.max + "," + this.variance

  def toTuple: (Int, Double, Double, T, T, Double) =
    (this.size, this.mean, this.median, this.min, this.max, this.variance)

}
