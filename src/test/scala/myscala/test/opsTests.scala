package myscala.test

import org.scalatest.FunSuite
import myscala.math.ops.TraversableOnceExtensions._

import scala.util.Random

class opsTests extends FunSuite {

  test("#1 minSKipNaN"){
    val data: Vector[Double] = Vector(1,2,3,Double.NaN)
    assert(data.minSkipNaN === 1.0 )
  }

  test("#2 maxSKipNaN"){
    val data: Vector[Double] = Vector(1,2,3,Double.NaN)
    assert(data.maxSkipNaN === 3.0 )
  }

  test("#3 minSKipNaN with random numbers"){
    val data: Vector[Double] = Vector.fill(Random.nextInt(500))(Random.nextDouble) ++ Vector(Double.NaN) ++ Vector(Double.NaN) ++ Vector(Double.NaN)
    assert(data.minSkipNaN === data.filterNot(_.isNaN).min )
  }

  test("#4 minSKipNaN with random numbers"){
    val data: Vector[Double] = Vector.fill(Random.nextInt(500))(Random.nextDouble) ++ Vector(Double.NaN) ++ Vector(Double.NaN) ++ Vector(Double.NaN)
    assert(data.maxSkipNaN === data.filterNot(_.isNaN).max )
  }

}
