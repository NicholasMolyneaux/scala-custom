package myscala

/**
  * Provides extensions for writing common scala types to text files.
  * Two formats are used: CSV and JSON. Although CSV is simpler, it can be used for
  * writing data to be read by TIKZ (latex). JSON is more flexible and is useful
  * when different models or analyses must share data.
  *
  * ==Overview==
  * The implicit functions are designed to be used like this:
  * {{{
  * val v: Vector[Double] = Vector(1,2,3,4,5)
  * v.writetoCSV("myVectorTestCSV.csv")
  * v.writeToJSON("travel_times", "myVectortestJSON.json")
  *
  * val m: Map[String,Vector[Double]] = Map("lausanne" -> Vector(1.1,2.0,3.7,4.3), "basel" -> Vector(9.0, 3.55, 3.2))
  * m.writeToCSV("myMapTestCSV.csv")
  * m.writeToJSON("train_stations", "myMaptestJSON.json")
  *
  * val ab: scala.collection.mustable.ArrayBuffer[(Int. Double)] = scala.collectection.mutable.ArrayBuffer((1,2.156),(2,3.1415),(3,42.23))
  * ab.writeToCSV("myABTestCSV.csv")
  * }}}
  */
package object output