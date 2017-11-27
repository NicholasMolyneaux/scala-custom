package myscala.math

import breeze.linalg.DenseVector

/**
  * Linear algebra functions
  *  - compute area based on 2D vectors
  *
  *  ==Overview==
  *  The main functions in this package are used as follows
  *  {{{
  *  import breeze.linalg.DenseVector
  *  val ab: DenseVector[Double] = DenseVector(2.0,0.0)
  *  val ad: DenseVector[Double] = DenseVector(0.0,3.0)
  *  areaFrom2DVectors(ab,ac)
  *  }}}
  *
  */
package object linalg {

  /** Computation of the area spanned by two vectors originating from the same point.
    * The order of the vectors does not matter.
    *
    * @param a first vector
    * @param b second vector
    * @return area spanned by both input vectors
    */
  def areaFrom2DVectors[T: Numeric](a: DenseVector[T], b: DenseVector[T]): T = {
    require(a.length == 2)
    require(b.length == 2)
    implicitly[Numeric[T]].abs(implicitly[Numeric[T]].minus(implicitly[Numeric[T]].times(a(0),b(1)), implicitly[Numeric[T]].times(a(1),b(0))))
  }
}
