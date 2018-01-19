package myscala.math

package object algo {

  trait PhysicalVector

  sealed class Vector2D(val X: Double, val Y: Double) extends PhysicalVector {

    def + (that: Vector2D): Vector2D = {new Vector2D(this.X + that.X, this.Y + that.Y)}
    def - (that: Vector2D): Vector2D = {new Vector2D(this.X - that.X, this.Y - that.Y)}

    def + (i: Double): Vector2D = {new Vector2D(this.X + i, this.Y + i)}
    def - (i: Double): Vector2D = {new Vector2D(this.X - i, this.Y - i)}
    def * (i: Double): Vector2D = {new Vector2D(this.X * i, this.Y * i)}
    def / (i: Double): Vector2D = {new Vector2D(this.X / i, this.Y / i)}

    override def toString: String = "(" + X + "," + Y + ")"
  }

  sealed class Vector3D(val X: Double, val Y: Double, val Z: Double) extends PhysicalVector {

    def + (that: Vector3D): Vector3D = {new Vector3D(this.X + that.X, this.Y + that.Y, this.Z + that.Z)}
    def - (that: Vector3D): Vector3D = {new Vector3D(this.X - that.X, this.Y - that.Y, this.Z - that.Z)}

    def + (i: Double): Vector3D = {new Vector3D(this.X + i, this.Y + i, this.Z + i)}
    def - (i: Double): Vector3D = {new Vector3D(this.X - i, this.Y - i, this.Z - i)}
    def * (i: Double): Vector3D = {new Vector3D(this.X * i, this.Y * i, this.Z * i)}
    def / (i: Double): Vector3D = {new Vector3D(this.X / i, this.Y / i, this.Z / i)}

    override def toString: String = "(" + X + "," + Y + ")"
  }

  def norm(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y), 0.5)
  def norm(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y) + (b.Z-a.Z)*(b.Z-a.Z), 0.5)



}