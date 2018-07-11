package myscala.math.vector

class Vector3D(val X: Double, val Y: Double, val Z: Double) extends PhysicalVector with PhysicalVectorOps[Vector3D] {

  def + (that: Vector3D): Vector3D = {new Vector3D(this.X + that.X, this.Y + that.Y, this.Z + that.Z)}
  def - (that: Vector3D): Vector3D = {new Vector3D(this.X - that.X, this.Y - that.Y, this.Z - that.Z)}

  def + (i: Double): Vector3D = {new Vector3D(this.X + i, this.Y + i, this.Z + i)}
  def - (i: Double): Vector3D = {new Vector3D(this.X - i, this.Y - i, this.Z - i)}
  def * (i: Double): Vector3D = {new Vector3D(this.X * i, this.Y * i, this.Z * i)}
  def / (i: Double): Vector3D = {new Vector3D(this.X / i, this.Y / i, this.Z / i)}

  def distanceTo(that: Vector3D): Double = scala.math.pow((that.X-this.X)*(that.X-this.X) + (that.Y-this.Y)*(that.Y-this.Y) + (that.Z-this.Z)*(that.Z-this.Z), 0.5)
  def norm: Double = scala.math.pow(this.X*this.X + this.Y*this.Y + this.Z*this.Z, 0.5)
  def dot(that: Vector3D): Double = this.X*that.X + this.Y*that.Y + this.Z*that.Z
  def normalized: Vector3D = new Vector3D(this.X / this.norm, this.Y / this.norm, this.Z / this.norm)

  override def toString: String = "(" + X + ", " + Y + ", " + Z + ")"

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Vector3D => that.canEqual(this) && this.X == that.X && this.Y == that.Y && this.Z == that.Z
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other Vector to compare to
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Vector3D]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.X, this.Y, this.Z).##
  }

}

object Vector3D {
  def apply(x: Double, y: Double, z: Double): Vector3D = new Vector3D(x, y, z)
}