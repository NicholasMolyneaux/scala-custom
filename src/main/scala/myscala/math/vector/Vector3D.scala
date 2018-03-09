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

}

object Vector3D {
  def apply(x: Double, y: Double, z: Double): Vector3D = new Vector3D(x, y, z)
}