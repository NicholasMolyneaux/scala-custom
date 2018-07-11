package myscala.output

import java.io.{BufferedWriter, File, FileWriter}

import play.api.libs.json.Json

/**
  * Function to write a Map[U, Seq[T]] to a CSV or JSON file. Also designed to be as generic as possible. Both type
  * parameters only require the toString method to exist.
  */
object MapSeqExtensions {

  /** implicit class for writing maps of sequences to files
    *
    * @param x data
    * @tparam U type of the keys
    * @tparam T type inside the Seq
    */
  implicit class MapVectorExtensionsWriter[U, T](x: scala.collection.Map[U, scala.collection.Seq[T]]) {

    /** Writes the map as a string in a recursive way. The vectors contained in the
      * map are written as columns and commas separate the values.
      *
      * @param mapVec data to transform
      * @param str string accumulator
      * @return all the vectors as a string.
      */
    private def reduceMapOfVectorsToString(mapVec: scala.collection.Map[U, scala.collection.Seq[T]], str: String): String = {
      if (mapVec.forall(p => p._2.isEmpty)) str
      else if (mapVec.forall(p => p._2.nonEmpty)) reduceMapOfVectorsToString(mapVec.map(p => (p._1, p._2.tail)), str + mapVec.map(p => p._2.head).mkString(",") + "\n")
      else if (mapVec.exists(p => p._2.nonEmpty)) {
        val strToAdd: String = mapVec.map(v => {
          if (v._2.nonEmpty) v._2.head.toString
          else "NaN"
        }).mkString(",")
        reduceMapOfVectorsToString(mapVec.map(p => p._1 -> {
          if (p._2.isEmpty) Seq[T]()
          else p._2.tail
        }), str + strToAdd + "\n")
      }
      else throw new Error("vectors of different sizes to print")
    }

    /** Writer to CSV where each column is a vector from the map and the column headers are the keys
      *
      * @param fileName name of the file to write to
      * @param path path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, path: String = ""): Unit = {

      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(x.keys.mkString(",") ++ "\n" ++ reduceMapOfVectorsToString(x, "").stripLineEnd)
      bw.close()
    }

    /** Writer of map of vectors to JSON. The JSON keys are the map's keys
      *
      * @param fieldName upper level json key
      * @param fileName name of the file
      * @param path path with trailing /, default is empty ""
      */
    def writeToJSON(fieldName: String, fileName: String, path: String = ""): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.obj(fieldName -> x.map(v => v._1.toString -> v._2.map(_.toString)))))
      bw.close()
    }
  }
}