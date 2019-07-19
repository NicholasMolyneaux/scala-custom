package myscala.test

import org.scalatest.FunSuite

import myscala.math.linalg._
import myscala.math.vector.Vector2D

class linalgTests extends FunSuite {

  test("#1 areaFrom2DVectors: simple unit"){
    val a = Vector2D(1.0, 0.0)
    val b = Vector2D(0.0, 1.0)
    assert(areaFrom2DVectors(a,b) === 1.0)
  }

  test("#2 areaFrom2DVectors: negative unit"){
    val a = Vector2D(-1.0, 0.0)
    val b = Vector2D(0.0, 1.0)
    assert(areaFrom2DVectors(a,b) === 1.0)
  }
}
