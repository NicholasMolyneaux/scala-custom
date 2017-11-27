package myscala.test

import breeze.linalg.DenseVector
import org.scalatest.FunSuite

import scala.util.Random
import myscala.math.linalg._

class linalgTests extends FunSuite {

  test("#1 areaFrom2DVectors: simple unit"){
    val a = DenseVector(1.0, 0.0)
    val b = DenseVector(0.0, 1.0)
    assert(areaFrom2DVectors(a,b) === 1.0)
  }

  test("#2 areaFrom2DVectors: negative unit"){
    val a = DenseVector(-1.0, 0.0)
    val b = DenseVector(0.0, 1.0)
    assert(areaFrom2DVectors(a,b) === 1.0)
  }
}
