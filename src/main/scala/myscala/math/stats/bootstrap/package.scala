package myscala.math.stats

import java.util.concurrent.ThreadLocalRandom

package object bootstrap {

  /** Performs a bootstrap on the data. The paramater to compute is passed as an argument. The number of permutations
    * to compute is also a parameter with a default value set to 100.
    *
    * @param data Sequence on which to perform the bootstrap
    * @param parameterFunction the parameter to compute on the data
    * @param r the number of permutations to compute, set to 100 by default
    * @tparam T type of the data
    * @return mean square error MSE
    */
  def bootstrapMSE[T: Numeric](data: Seq[T], parameterFunction: Seq[T] => Double, r: Int = 100): Bootstrap[T] = {

    // The parameter computed on the original data
    val param: Double = parameterFunction(data)

    /** Sample with replacement the data into a collection of the same size as the original data.
      *
      * @param data data to resample
      * @return Same size resampled collection
      */
    def sampleWithReplacement(data: Seq[T]): Seq[T] = {
      Vector.fill(data.size)(ThreadLocalRandom.current().nextInt(data.size)).map(v => data(v))
    }

    // Performs the bootstrap and computes the MSE
    val MSE: Double = (0.to(r).by(1).foldLeft(0.0)((s, v) => s + math.pow(parameterFunction(sampleWithReplacement(data))-param, 2)))/r

    Bootstrap(MSE, param, parameterFunction)
  }

}
