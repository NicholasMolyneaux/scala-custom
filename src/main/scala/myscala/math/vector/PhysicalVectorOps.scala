package myscala.math.vector

trait PhysicalVectorOps[A] {

  def + (that: A): A
  def - (that: A): A

  def + (d: Double): A
  def - (d: Double): A
  def * (d: Double): A
  def / (d: Double): A

  def norm: Double
  def distanceTo(that: A): Double
  def dot(that: A): Double
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
