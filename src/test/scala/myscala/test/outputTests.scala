package myscala.test

/**
  * Created by nicholas on 3/26/17.
  */

import org.scalatest.FunSuite

import io.{BufferedSource, Source}
import myscala.output.SeqExtension.SeqWriter
import myscala.output.MapSeqExtensions._
import myscala.output.SeqTuplesExtensions._
import myscala.output.SeqOfSeqExtensions._
import myscala.input.ReadJson._
import org.junit.Assert._


//@RunWith(classOf[JUnitRunner])
class outputTests extends FunSuite {

  test("#1 file produced from writeToCSV is same as writeToCSV-ints-expected.csv") {
    // create vector[int] and write to file
    val v:Vector[Int] = Vector(1,2,3,4,5,6,7,8,9,10)
    v.writeToCSV("writeToCSV-ints.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-ints.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-ints-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#2 file produced from writeToCSV is same as writeToCSV-doubles-expected.csv") {
    // create vector[int] and write to file
    val v:Vector[Double] = Vector(1.0, 2.0, 3.0,4,5,6,7,8,9,10)
    v.writeToCSV("writeToCSV-doubles.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-doubles.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-doubles-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#3 file produced from writeToCSV of Map[U, Vector[Int]] is same as expected") {
    // create vector[int] and write to file
    val v:Map[String, Vector[Int]] = Map("a" -> Vector(1,2,3,4,5), "b" -> Vector(6,7,8,9,10), "c" -> Vector(11,12,13,14,15))
    v.writeToCSV("writeToCSV-map-ints.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-map-ints.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-map-ints-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#4 file produced from writeToCSV of Map[U, Vector[Double]] is same as expected") {
    // create vector[int] and write to file
    val v: Map[String, Vector[Double]] = Map(
      "a" -> Vector(1.5,2,3,4.75,5),
      "b" -> Vector(6,7,8.99,9,10),
      "c" -> Vector(11.09,12,13.01,14,15))
    v.writeToCSV("writeToCSV-map-doubles.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-map-doubles.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-map-doubles-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#5 file produced from writeToCSV of Map[U, Vector[Double]] of different size vectors") {
    // create vector[int] and write to file
    val v:Map[String, scala.collection.mutable.ArrayBuffer[Double]] = Map(
      "a" -> scala.collection.mutable.ArrayBuffer(1.5,2,3),
      "b" -> scala.collection.mutable.ArrayBuffer(6,7,8.99,9,10),
      "c" -> scala.collection.mutable.ArrayBuffer(4,15)
    )
    v.writeToCSV("writeToCSV-map-different-size-vectors.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-map-different-size-vectors.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-map-different-size-vectors-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#6 file produced from writeToJSON of Vector[Int]") {
    // create vector[int] and write to file
    val v:Vector[Int] = Vector(1,2,3,4,5,6,7,8,9,10)
    v.writeToJSON("scalatest", "writeToJSON-ints.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val inputExpected = readVector(getClass.getResource("/writeToJSON-ints-expected.csv").getFile)
    val expected = inputExpected.get.flatMap(c => c.v.map(_.toInt))

    val inputTest = readVector(getClass.getResource("/writeToJSON-ints.csv").getFile)
    val test = inputTest.get.flatMap(c => c.v.map(_.toInt))

    assertEquals(expected.size, test.size)
    assert((for (i <- expected.indices) yield {
      test.contains(expected(i)) && expected.contains(test(i))
    }).forall(b => b))
  }

  test("#7 file produced from writeToJSON of Vector[Double]") {
    // create vector[int] and write to file
    val v:Vector[Double] = Vector(1.1, 2.01, 3.0005,4.99,5.0,6,7,8,9,10.10)
    v.writeToJSON("scalatest", "writeToJSON-doubles.csv", getClass.getResource("/").getPath)

    val inputExpected = readVector(getClass.getResource("/writeToJSON-doubles-expected.csv").getFile)
    val expected = inputExpected.get.flatMap(c => c.v.map(_.toDouble))

    val inputTest = readVector(getClass.getResource("/writeToJSON-doubles.csv").getFile)
    val test = inputTest.get.flatMap(c => c.v.map(_.toDouble))

    assertEquals(expected.size, test.size)
    assert((for (i <- expected.indices) yield {
      test.contains(expected(i)) && expected.contains(test(i))
    }).forall(b => b))
  }

  test("#8 file produced from writeToJSON of Map[U, Vector[Double]] of different size vectors to tolerance of 10^-5") {
    // create vector[int] and write to file
    val v:Map[String, Vector[Double]] = Map("a" -> Vector(1.5,2,3), "b" -> Vector(6,7,8.99,9,10), "c" -> Vector(4,15))
    v.writeToJSON("scalatest", "writeToJSON-map-different-size-vectors.csv", getClass.getResource("/").getPath)

    val inputExpected = readMultipleVectors(getClass.getResource("/writeToJSON-map-different-size-vectors-expected.csv").getFile)
    val expected: (String, Map[String, List[Double]]) = (inputExpected.get.str, inputExpected.get.v.map(c => c.str -> c.v.map(_.toDouble)).toMap)

    val inputTest = readMultipleVectors(getClass.getResource("/writeToJSON-map-different-size-vectors.csv").getFile)
    val test: (String, Map[String, List[Double]]) = (inputTest.get.str, inputTest.get.v.map(c => c.str -> c.v.map(_.toDouble)).toMap)

    assertEquals(expected._1, test._1)
    assertEquals(expected._2.size, test._2.size)

    val expectedKeys: Seq[String] = expected._2.keys.toSeq
    val testKeys: Seq[String] = test._2.keys.toSeq

    assert((for (i <- expectedKeys.indices) yield {
      testKeys.contains(expectedKeys(i)) && expectedKeys.contains(testKeys(i))
    }).forall(b => b))

    for (i <- expectedKeys) yield {
      assertArrayEquals(expected._2(i).toArray, test._2(i).toArray, 0.00001)
    }
  }

  test("#9 file produced from writeToCSV of ArrayBuffer[(Int, Int)]") {
    // create vector[int] and write to file
    val ab:scala.collection.mutable.ArrayBuffer[(Int, Int)] = scala.collection.mutable.ArrayBuffer((1,10),(2,20),(3,30),(4,40),(5,50),(6,60),(7,70),(8,80),(9,90),(10,100))
    ab.writeToCSV("writeToCSV-ArrayBuffer-tuple-int-int.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-ArrayBuffer-tuple-int-int.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-ArrayBuffer-tuple-int-int-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#10 file produced from writeToCSV of ArrayBuffer[(Int, Double)]") {
    // create vector[int] and write to file
    val ab:scala.collection.mutable.ArrayBuffer[(Int, Double)] = scala.collection.mutable.ArrayBuffer((1,1.123),(2,2.0),(3,3.99),(4,4.250),(5,50))
    ab.writeToCSV("writeToCSV-ArrayBuffer-tuple-int-double.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-ArrayBuffer-tuple-int-double.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-ArrayBuffer-tuple-int-double-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#11 file produced from writeToCSV of Seq[Seq[Int]]") {
    // create vector[int] and write to file
    val ss: List[scala.collection.mutable.ArrayBuffer[Int]] = List(
      scala.collection.mutable.ArrayBuffer(1,2,3),
      scala.collection.mutable.ArrayBuffer(4,5,6),
      scala.collection.mutable.ArrayBuffer(7,8,9),
      scala.collection.mutable.ArrayBuffer(10,11,12),
      scala.collection.mutable.ArrayBuffer(13,14,15))
    ss.writeToCSV("writeToCSV-SeqSeq-int.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-int.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-int-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#12 file produced from writeToCSV of Seq[Seq[Double]]") {
    // create vector[int] and write to file
    val ss: Vector[scala.collection.immutable.Queue[Double]] = Vector(
      scala.collection.immutable.Queue(1.5,2.46,3435.0),
      scala.collection.immutable.Queue(42.4,5,6),
      scala.collection.immutable.Queue(7.0,8,9.26),
      scala.collection.immutable.Queue(10,11.123,12),
      scala.collection.immutable.Queue(13,14,15))
    ss.writeToCSV("writeToCSV-SeqSeq-double.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-double.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-double-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#13 file produced from writeToCSV of Seq[Seq[Double]] with row and column names") {
    // create vector[int] and write to file
    val ss: Vector[scala.collection.immutable.Queue[Double]] = Vector(
      scala.collection.immutable.Queue(1.5,2.46,3435.0),
      scala.collection.immutable.Queue(42.4,5,6),
      scala.collection.immutable.Queue(7.0,8,9.26),
      scala.collection.immutable.Queue(10,11.123,12),
      scala.collection.immutable.Queue(13,14,15))
    ss.writeToCSV("writeToCSV-SeqSeq-withNames-double.csv", Option(Vector("a", "b", "c")), Option(Vector("id", "1", "2", "3","4", "5")), getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-withNames-double.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-withNames-double-expected.csv")).mkString
    assertEquals(expected, test)
  }

  test("#14 file produced from writeToCSV of Seq[Seq[Double]] with uneven lengths") {
    // create vector[int] and write to file
    val ss: Vector[scala.collection.immutable.Queue[Double]] = Vector(
      scala.collection.immutable.Queue(1.5,2.46,3435.0, 56),
      scala.collection.immutable.Queue(42.4,5,6),
      scala.collection.immutable.Queue(7.0,8,9.26,3,4,5),
      scala.collection.immutable.Queue(10,11.123,12),
      scala.collection.immutable.Queue(13,14))
    ss.writeToCSV("writeToCSV-SeqSeq-double-uneven.csv", getClass.getResource("/").getPath)

    // read files into strings and compare them
    val test = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-double-uneven.csv")).mkString
    val expected = Source.fromURL(getClass.getResource("/writeToCSV-SeqSeq-double-uneven-expected.csv")).mkString
    assertEquals(expected, test)
  }
}

