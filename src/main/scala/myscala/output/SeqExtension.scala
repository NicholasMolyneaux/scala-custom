package myscala.output

import java.io.{BufferedWriter, File, FileWriter}

import play.api.libs.json.Json

/**
  * Extensions of scala.collection.Seq, a parent of many collections classes which look like a Vector. The only
  * requirement is the existance of a toString method. As this always exists (if no specific method exists, the
  * address in memory is printed) these implicit functions are very generic. These functions work on both mutable
  * and immutable collections.
  */
object SeqExtension {

  /** Implicit class for writers of Seq[T]
    * assumes that a mkString method exists for template T
    *
    * @param x vector to write to row csv
    * @tparam T type of object to write
    */
  implicit class SeqWriter[T](x: scala.collection.Seq[T]) {

    /**
      * Method for writing Seq[T] to column csv. Basically only a single column of data.
      * {{{
      * val vec: Vector = Vector(1,2,3,4)
      * vec.writeToCSV("test.csv")
      * }}}
      *
      * @param fileName the name of the file
      * @param path     path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, path: String = ""): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(x.mkString("\n")) // + "\n")
      bw.close()
    }

    /**
      * Function to write Seq of data to JSON file. The key for the JSON is passed
      * as the first argument, while the second one is the file name.
      * {{{
      * val vec: Vector = Vector(1,2,3,4)
      * vec.writeToJSON("sample", "test.csv")
      * }}}
      *
      * @param fieldName JSON requires a key
      * @param fileName  the name of the file
      * @param path      the path with trailing /, default is empty ""
      */
    def writeToJSON(fieldName: String, fileName: String, path: String = ""): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      val m: Map[String, Seq[String]] = Map(fieldName -> x.map(_.toString))
      bw.write(Json.prettyPrint(Json.toJson(m)))
      bw.close()
    }
  }
}
