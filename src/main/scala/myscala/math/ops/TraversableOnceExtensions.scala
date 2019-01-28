package myscala.math.ops

object TraversableOnceExtensions {

  implicit class MinMax(x: TraversableOnce[Double]) {

    def minSkipNaN(implicit cmp: Ordering[Double]): Double = {
      if (x.isEmpty) {throw new UnsupportedOperationException("empty.minSkipNaN")}
      x.reduceLeft((x, y) => {
        y match {
          case a if a.isNaN || a.isInfinity => x
          case b if cmp.lteq(x, y) => x
          case _ => y
        }
      })
    }

    def maxSkipNaN(implicit cmp: Ordering[Double]): Double = {
      if (x.isEmpty) {throw new UnsupportedOperationException("empty.maxSkipNaN")}
      x.reduceLeft((x, y) => {
        y match {
          case a if a.isNaN || a.isInfinity => x
          case b if cmp.gteq(x, y) => x
          case _ => y
        }
      })    }

  }

}
