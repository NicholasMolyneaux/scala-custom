package myscala.test

/**
  * Created by nicholas on 3/31/17.
  */

import org.scalatest.FunSuite
import scala.util.Random
import myscala.math.stats._

class statsTests extends FunSuite {

  test("#1 computeQuantiles: 0 quantile is min of data"){
    val v = Seq.fill(100)(Random.nextDouble)
    val q = computeQuantiles(Seq(0))(v)
    assert(v.min === q.values.head)
  }

  test("#2 computeQuantiles: 50 quantile is median of data"){
    val v = Vector.fill(101)(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantiles(Seq(50))(v)
    assert(vSorted(50) === q.values.head)
  }

  test("#3 computeQuantiles: 100 quantile is max of data"){
    val v = Seq.fill(100)(Random.nextDouble)
    val q = computeQuantiles(Seq(100))(v)
    assert(v.max === q.values.head)
  }

  test("#4 computeQuantiles: values are in increasing order"){
    val v = Seq.fill(100)(Random.nextDouble)
    val q = computeQuantiles(scala.collection.immutable.Range.Double(10,100,10))(v)
    assert(q.values === q.values.sorted)
  }

  test("#5 computeQuantiles: 99.9 percentile"){
    val v = Seq.fill(536)(Random.nextDouble)
    val q = computeQuantiles(Seq(99.8))(v)
    val vSorted = v.sorted
    assert(q.values.head === vSorted(534))
  }

  test("#6 computeQuantiles: 0,50,100"){
    val v = Seq.fill(123)(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantiles(Seq(0,50,100))(v)
    assert(q.values === Seq(vSorted.min, vSorted(61), vSorted.max))
  }

  test("#7 computeQuantile: 50 is median"){
    val v = Vector.fill(101)(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantile(50)(v)
    assert(vSorted(50) === q.value)
  }
}
