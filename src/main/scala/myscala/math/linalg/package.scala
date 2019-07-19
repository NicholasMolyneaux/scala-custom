package myscala.math

import myscala.math.vector.Vector2D

/**
  * Linear algebra functions
  *  - compute area based on 2D vectors
  */
package object linalg {

  /** Computation of the area spanned by two vectors originating from the same point.
    * The order of the vectors does not matter.
    *
    * @param a first vector
    * @param b second vector
    * @return area spanned by both input vectors
    */
  def areaFrom2DVectors(a: Vector2D, b: Vector2D): Double = {
    math.abs(a.X * b.Y - a.Y * b.X)
  }
}
