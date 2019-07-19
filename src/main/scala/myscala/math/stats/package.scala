package myscala.math

/**
  * Statistical functions
  *  - quantile computation
  *
  *  ==Overview==
  *  The main functions in this package are used as follows
  *  {{{
  *  import scala.util.Random
  *
  *  // Computation of quantiles example
  *  val computeQuantilesEvery10 = computeQuantiles(Seq(10,20,30,40,50,60,70,80,90))_ // partial evaluation (currying)
  *  val q = computeQuantilesEvery10(Seq.fill(123)(Random.nextDouble))
  *
  *  // Statistics example
  *  val data = Vector(1,2,3,4,5,6,7,8,9,10)s
  *  data.statistics // thanks to the implicit class
  *  }}}
  *
  */
package object stats {

  /** Returns a Quantiles object with the values of the quantiles
    *
    * @param listQuantiles the set of quantiles to use
    * @param data the data
    * @return a Quantiles object
    */
  def computeQuantiles[T: Numeric](listQuantiles: Seq[Double])(data: scala.collection.immutable.Seq[T]): Quantiles[T] = {
    val sortedSeq: scala.collection.immutable.Seq[T] = data.sortWith((a,b) => implicitly[Numeric[T]].lt(a,b))
    def calculatePercentile(p: Double): T = sortedSeq(math.ceil((data.length - 1) * (p / 100.0)).toInt)
    val sortedQuantiles: Seq[Double] = listQuantiles.sorted
    Quantiles(sortedQuantiles, sortedQuantiles.map(v => calculatePercentile(v)), data.size)
  }

  /** Computes a specific quantile of the data
    *
    * @param quantile where the quantile should be computed
    * @param data the data
    * @return a Quantile object
    */
  def computeQuantile[T: Numeric](quantile: Double)(data: Seq[T]): Quantile[T] = {
    val sortedSeq: Seq[T] = data.sortWith((a,b) => implicitly[Numeric[T]].lt(a,b))
    Quantile(quantile,sortedSeq(math.ceil((data.length - 1) * (quantile / 100.0)).toInt), data.size)
  }

  /**
    *
    * @param x collection of Numeric type
    * @tparam T must be Numeric
    */
  implicit class ComputeQuantiles[T: Numeric](x: scala.collection.immutable.Seq[T]) {

    val num: Numeric[T] = implicitly[Numeric[T]]
    import num.mkOrderingOps

    /**
      * Returns the distribution without the values strictly above the quantile specified as argument.
      * @param q quantile above which to cut off
      * @return original collection without values above quantile q
      */
    def cutOfAfterQuantile(q: Double): scala.collection.Seq[T] = {
      val valueAtQuantile: T = myscala.math.stats.computeQuantile(q)(x).value
      x.filter(v => v <= valueAtQuantile)
    }

    /**
      * Returns the distribution without the values strictly below the quantile specified as argument.
      * @param q quantile below which to cut off
      * @return original collection without values below quantile q
      */
    def cutOfBeforeQuantile(q: Double): scala.collection.Seq[T] = {
      val valueAtQuantile: T = myscala.math.stats.computeQuantile(q)(x).value
      x.filter(v => v >= valueAtQuantile)
    }
  }


  /** Computes basic statistics of a collection of numeric type. The statistics are the following:
    * mean, variance, median, max, min.
    *
    * @param v collection of numeric type
    * @tparam T numeric type
    * @return (size, mean, var, median, min, max)
    */
  def statistics[T: Numeric](v: scala.collection.Iterable[T]): Statistics[T] = {

    if (v.nonEmpty) {
      // mean and variance computation
      val mean: Double = implicitly[Numeric[T]].toDouble(v.sum) * (1.0 / v.size)
      val diff2Mean: Vector[Double] = v.map(implicitly[Numeric[T]].toDouble(_) - mean).toVector

      // median computation
      val (lower: Seq[T], upper: Seq[T]) = v.toVector.sorted.splitAt(v.size / 2)
      val median: Double = if (v.size % 2 == 0) {
        (implicitly[Numeric[T]].toDouble(lower.last) + implicitly[Numeric[T]].toDouble(upper.head)) / 2.0
      } else {
        implicitly[Numeric[T]].toDouble(upper.head)
      }

      Statistics(v.size, mean, median, v.min(implicitly[Numeric[T]]), v.max(implicitly[Numeric[T]]), math.sqrt(diff2Mean.map(v => v * v).sum / (v.size - 1.0)))
    } else if (v.size == 1) {
      Statistics(v.size, implicitly[Numeric[T]].toDouble(v.head), implicitly[Numeric[T]].toDouble(v.head), v.head, v.head, Double.NaN)
    } else {
      Statistics(0, Double.NaN, Double.NaN, Double.NaN.asInstanceOf[T], Double.NaN.asInstanceOf[T], Double.NaN)
    }
  }

  @deprecated("Will be removed", "1.3.0")
  def stats[T: Numeric](v: scala.collection.Iterable[T]): (Int, Double, Double, Double, T, T) = {

    if (v.nonEmpty) {
      // mean and variance computation
      val mean: Double = implicitly[Numeric[T]].toDouble(v.sum) * (1.0 / v.size)
      val diff2Mean: Vector[Double] = v.map(implicitly[Numeric[T]].toDouble(_) - mean).toVector

      // median computation
      val (lower: Seq[T], upper: Seq[T]) = v.toVector.sorted.splitAt(v.size / 2)
      val median: Double = if (v.size % 2 == 0) {
        (implicitly[Numeric[T]].toDouble(lower.last) + implicitly[Numeric[T]].toDouble(upper.head)) / 2.0
      } else {
        implicitly[Numeric[T]].toDouble(upper.head)
      }

      (v.size, mean, math.sqrt(diff2Mean.map(v => v * v).sum / (v.size - 1.0)), median, v.min(implicitly[Numeric[T]]), v.max(implicitly[Numeric[T]]))
    } else if (v.size == 1) {
      (v.size, implicitly[Numeric[T]].toDouble(v.head), Double.NaN, implicitly[Numeric[T]].toDouble(v.head), v.head, v.head)
    } else {
      (0, Double.NaN, Double.NaN, Double.NaN, Double.NaN.asInstanceOf[T], Double.NaN.asInstanceOf[T])
    }
  }

  implicit class ComputeStats[T: Numeric](x: scala.collection.Iterable[T]) {
    def statistics: Statistics[T] = myscala.math.stats.statistics(x)

    @deprecated("Will be removed", "1.3.0")
    def stats: (Int, Double, Double, Double, T ,T) = myscala.math.stats.stats(x)
  }



  /** Computes the data for plotting a box plot. The input is a Seq of Numeric type. The output is a [[BoxPlot]] object
    * containing the results.
    *
    * The whiskers are defined in the pgfplots manual in the section about boxplots (https://ctan.org/pkg/pgfplots?lang=en).
    *
    * @param data Seq of numeric type to compute the boxplot data for.
    * @return [[BoxPlot]] object
    */
  def computeBoxPlotData[T: Numeric](data: scala.collection.immutable.Seq[T]): BoxPlot[T] = {

    val num: Numeric[T] = implicitly[Numeric[T]]
    import num.mkNumericOps

    val mean: Double = data.sum.toDouble/data.size

    val quarters = computeQuantiles(Vector(25.0, 50.0, 75.0))(data)
    val lowerQuartile = quarters.values(0)
    val median = quarters.values(1)
    val upperQuartile = quarters.values(2)

    val lowerQuartileDouble = lowerQuartile.toDouble
    val medianDouble = median.toDouble
    val upperQuartileDouble  = upperQuartile.toDouble

    val lowerWhisker: Double = data.map(_.toDouble - (lowerQuartileDouble - (upperQuartileDouble - lowerQuartileDouble) * 1.5)).filter(_ >= 0).min + lowerQuartileDouble - (upperQuartileDouble - lowerQuartileDouble) * 1.5
    val upperWhisker: Double = data.map(_.toDouble - (upperQuartileDouble + (upperQuartileDouble - lowerQuartileDouble) * 1.5)).filter(_ <= 0).max + upperQuartileDouble + (upperQuartileDouble - lowerQuartileDouble) * 1.5

    BoxPlot(mean, medianDouble, lowerQuartile, upperQuartile, lowerWhisker, upperWhisker, data.filter(v => v.toDouble < lowerWhisker || v.toDouble > upperWhisker)
    )
  }
}


