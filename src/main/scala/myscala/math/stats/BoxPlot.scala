package myscala.math.stats

/** Container for the results from the boxplot computation.
  *
  * The whiskers are computed as in the pfgplots package (https://ctan.org/pkg/pgfplots?lang=en).
  *
  * @param mean mean of the data
  * @param median median of the data
  * @param upperQuartile lower quartile
  * @param lowerQuartile upper quartile
  * @param lowerWhisker lower whisker
  * @param upperWhisker upper whisker
  * @param outliers collection of outliers
  * @tparam T type of data
  */
case class BoxPlot[T: Numeric](mean: Double, median: Double, lowerQuartile: T, upperQuartile: T, lowerWhisker: Double, upperWhisker: Double, outliers: Seq[T]) {

  override def toString: String = "Box plot results. Mean: " + this.mean + ", median: " + this.median + ", LQ: " + this.lowerQuartile +
    ", UQ: " + this.upperQuartile + ", LW: " + this.lowerWhisker + ", UW: " + this.upperWhisker + ", outlier count: " + this.outliers.size

  val CSVColumnNames: String = "mean,median,lq,uq,lw,uw,outliersize"

  def toCSV: String =
    this.mean + ", " + this.median + ", " + this.lowerQuartile + "," + this.upperQuartile + "," + this.lowerWhisker + "," + this.upperWhisker + "," + this.outliers.size

  def toTuple: (Double, Double, T, T, Double, Double, Int) =
    (this.mean , this.median , this.lowerQuartile , this.upperQuartile , this.lowerWhisker , this.upperWhisker , this.outliers.size)
}
