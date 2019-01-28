package myscala.math.vector

/**
  * Two dimensional simple vector class. Simple operations like + and - are defined for vector to vector, while
  * multiplication, addition, division and subtraction are defined for scalars. Standard operations like norm,
  * dot product and orthogonal exist as well.
  *
  * @param X first coordinate defined using Double
  * @param Y second coordinate defined using Double
  */
class Vector2D(val X: Double, val Y: Double) extends PhysicalVector with PhysicalVectorOps[Vector2D] {


  def + (that: Vector2D): Vector2D = {new Vector2D(this.X + that.X, this.Y + that.Y)}

  def - (that: Vector2D): Vector2D = {new Vector2D(this.X - that.X, this.Y - that.Y)}


  def + (i: Double): Vector2D = {new Vector2D(this.X + i, this.Y + i)}


  def - (i: Double): Vector2D = {new Vector2D(this.X - i, this.Y - i)}


  def * (i: Double): Vector2D = {new Vector2D(this.X * i, this.Y * i)}


  def / (i: Double): Vector2D = {new Vector2D(this.X / i, this.Y / i)}


  def norm: Double = scala.math.pow(this.X*this.X + this.Y*this.Y, 0.5)


  def distanceTo(that: Vector2D): Double = scala.math.pow((that.X-this.X)*(that.X-this.X) + (that.Y-this.Y)*(that.Y-this.Y), 0.5)

  def dot(that: Vector2D): Double = this.X*that.X + this.Y*that.Y

  def normalized: Vector2D = if (this.norm == 0.0) {throw new RuntimeException("Norm is zero !")} else {val n: Double = this.norm; new Vector2D(this.X / n, this.Y / n)}

  /**
    * Returns one of the two possible orthogonal vectors NORMALIZED.
    * @return
    */
  def orthogonal: Vector2D = new Vector2D(-this.Y, this.X).normalized

  /**
    * Prints the vector as a string such as (X, Y).
    * @return string representation
    */
  override def toString: String = "(" + X + ", " + Y + ")"

  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: Vector2D => that.canEqual(this) && this.X == that.X && this.Y == that.Y
      case _ => false
    }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other Vector to compare to
    * @return
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Vector2D]


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    (this.X, this.Y).##
  }
}

/**
  * Companion object of the [[Vector2D]] class. The apply method is defined such that the "new" keyword
  * can be omitted.
  */
object Vector2D {

  /**
    * Makes the usae of the "new" keyword redundant.
    * @param x x coordinates
    * @param y y coordinate
    * @return [[Vector2D]]
    */
  def apply(x: Double, y: Double): Vector2D = new Vector2D(x,y)
}