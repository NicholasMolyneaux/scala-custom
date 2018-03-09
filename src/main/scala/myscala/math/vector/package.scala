package myscala.math

package object vector {


  def norm(a: Vector2D, b: Vector2D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y), 0.5)
  def norm(a: Vector3D, b: Vector3D): Double = scala.math.pow((b.X-a.X)*(b.X-a.X) + (b.Y-a.Y)*(b.Y-a.Y) + (b.Z-a.Z)*(b.Z-a.Z), 0.5)



}
