package myscala.math

/**
  * Statistical functions
  *  - quantile computation
  *
  *  ==Overview==
  *  The main functions in this package are used as follows
  *  {{{
  *  import scala.util.Random
  *  val computeQuantilesEvery10 = computeQuantiles(Seq(10,20,30,40,50,60,70,80,90))_ // partial evaluation (currying)
  *  val q = computeQuantilesEvery10(Seq.fill(123)(Random.nextDouble))
  *  }}}
  *
  */
package object stats {

  /** Returns a Quantiles object with the values of the quantiles
    *
    * @param q the set of quantiles to use
    * @param data the data
    * @return a Quantiles object
    */
  def computeQuantiles[T: Numeric](q: Seq[Double])(data: Seq[T]): Quantiles[T] = {
    val sortedSeq: Seq[T] = data.sortWith((a,b) => implicitly[Numeric[T]].lt(a,b))
    def calculatePercentile(p: Double): T = sortedSeq(math.ceil((data.length - 1) * (p / 100.0)).toInt)
    Quantiles(q, q.map(v => calculatePercentile(v)), data.size)
  }

  /** Computes a specific quantile of the data
    *
    * @param q where the quantile should be computed
    * @param data the data
    * @return a Quantile object
    */
  def computeQuantile[T: Numeric](q: Double)(data: Seq[T]): Quantile[T] = {
    val sortedSeq: Seq[T] = data.sortWith((a,b) => implicitly[Numeric[T]].lt(a,b))
    Quantile(q,sortedSeq(math.ceil((data.length - 1) * (q / 100.0)).toInt), data.size)
  }

  //type StatsResults[T: Numeric] = (Int, Double, Double, Double, Double, Double)

  /** Computes basic statistics of a collection of numeric type. The statistics are the following:
    * mean, variance, median, max, min.
    *
    * @param v collection of numeric type
    * @tparam T numeric type
    * @return (size, mean, var, median, min, max)
    */
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
    } else {
      (0, Double.NaN, Double.NaN, Double.NaN, Double.NaN.asInstanceOf[T], Double.NaN.asInstanceOf[T])
    }
  }

  implicit class ComputeStats[T: Numeric](x: scala.collection.Iterable[T]) {
    def stats: (Int, Double, Double, Double, T, T) = myscala.math.stats.stats(x)
  }
}


