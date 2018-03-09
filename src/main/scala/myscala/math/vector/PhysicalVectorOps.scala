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

}
