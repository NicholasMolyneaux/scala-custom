package myscala.math.vector

trait PhysicalVectorOps[A] {

  /**
    * summation of two [[PhysicalVector]]
    * @param that the other [[PhysicalVector to sum]]
    * @return [[PhysicalVector]] equal to the sum of both vectors
    */
  def + (that: A): A

  /**
    * Subtraction of a [[Vector2D]] from this vector.
    * @param that the [[Vector2D]] to subtract from this vector
    * @return the result of this minus that
    */
  def - (that: A): A

  /**
    * Scalar summation. Adds a scala to both components.
    * @param d scalar to add
    * @return [[PhysicalVector]] of the sum
    */
  def + (d: Double): A

  /**
    * Scalar subtraction. Removes the argument from this vector
    * @param d scala to remove
    * @return the difference between this and that scalar
    */
  def - (d: Double): A

  /**
    * Scalar multiplication. Multiples both components by the argument.
    * @param d Scalar to multiply by
    * @return [[PhysicalVector]] after the multiplication
    */
  def * (d: Double): A

  /**
    * Division by a scalar.
    * @param d Scalar to divide this by
    * @return result from the division
    */
  def / (d: Double): A

  /**
    * Computes the L2 norm of this vector.
    * @return Double
    */
  def norm: Double

  /**
    * Computes the distance between this and that assuming both [[PhysicalVector]] represent points.
    * @param that point to compute the distance to
    * @return L2 distance between both points
    */
  def distanceTo(that: A): Double

  /**
    * Copmutes the dot product between both vectors.
    * @param that [[PhysicalVector]] to compute the dot product with
    * @return dot product between both vectors
    */
  def dot(that: A): Double

  /**
    * Returns the same vector except with unit length.
    * @return
    */
  def normalized: A

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other is the comparisoin legal
    * @return
    */
  def canEqual(other: Any): Boolean


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int
}
