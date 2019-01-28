package myscala.test

/**
  * Created by nicholas on 3/31/17.
  */

import org.scalatest.FunSuite
import scala.util.Random
import myscala.math.stats._

class statsTests extends FunSuite {

  test("#1 computeQuantiles: 0 quantile is min of data"){
    val v = Seq.fill(Random.nextInt(500))(Random.nextDouble)
    val q = computeQuantiles(Seq(0))(v)
    assert(v.min === q.values.head)
  }

  test("#2 computeQuantiles: 50 quantile is median of data"){
    val v = Vector.fill(100)(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantiles(Seq(50))(v)
    assert(vSorted(50) === q.values.head)
  }

  test("#3 computeQuantiles: 100 quantile is max of data"){
    val v = Seq.fill(Random.nextInt(500))(Random.nextDouble)
    val q = computeQuantiles(Seq(100))(v)
    assert(v.max === q.values.head)
  }

  test("#4 computeQuantiles: values are in increasing order"){
    val v = Seq.fill(Random.nextInt(500))(Random.nextDouble)
    val q = computeQuantiles(scala.collection.immutable.Range.Double(10,100,10))(v)
    assert(q.values === q.values.sorted)
  }

  test("#6 computeQuantiles: 0,50,100"){
    val v = Seq.fill(Random.nextInt(500))(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantiles(Seq(0,50,100))(v)
    assert(q.values === Seq(vSorted.min, vSorted((v.size*0.5).toInt), vSorted.max))
  }

  test("#7 computeQuantile: 50 is median"){
    val v = Vector.fill(Random.nextInt(500))(Random.nextDouble)
    val vSorted = v.sorted
    val q = computeQuantile(50)(v)
    assert(vSorted((v.size*0.5).toInt) === q.value)
  }

  test("#8 stats on Vector[Double]"){
    val v: Vector[Double] = Vector.fill(Random.nextInt(500))(Random.nextDouble)
    val mean: Double = v.sum/v.size
    val minimum: Double = v.min
    val maximum: Double = v.max
    val size: Int = v.size

    // median
    val diff2Mean: Vector[Double] = v.map(_ - mean)
    val variance = math.sqrt(diff2Mean.map(v => v * v).sum / (v.size - 1.0))

    // median computation
    val (lower, upper) = v.sorted.splitAt(v.size / 2)
    val median: Double = if (v.size % 2 == 0) {
      (lower.last +upper.head) / 2.0
    } else {
      upper.head
    }

    val computedStats = v.stats
    assert(size === computedStats._1 &&
      math.abs(mean-computedStats._2) < math.pow(10,-8) &&
      math.abs(variance-computedStats._3) < math.pow(10,-8) &&
      math.abs(median-computedStats._4) < math.pow(10,-8) &&
      math.abs(minimum-computedStats._5) < math.pow(10,-8) &&
      math.abs(maximum-computedStats._6) < math.pow(10,-8))
  }
}
