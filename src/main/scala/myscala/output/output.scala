package myscala.output


import play.api.libs.json._
import java.io.{BufferedWriter, File, FileWriter}

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


/**
  * Extensions of scala.collection.Seq with tuples
  */
object SeqTuplesExtensions {

  /** Implicit class for writers of ArrayBuffer[(T, U)]
    * assumes that a mkString method exists for template parameters T and U.
    * If this is not the case, an error will likely appear.
    *
    * @param x ArrayBuffer of tuple of size 2 to write to column csv
    */
  implicit class SeqTuplesWriter(x: scala.collection.Seq[Product]) {

    /** method for writing ArrayBuffer[(T, U)]
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      */
    def writeToCSV(fileName: String): Unit = {
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      val str: String = x.map(t => t.productIterator.toIndexedSeq.map(_.toString).mkString(",")).mkString("\n")
      bw.write(str)
      bw.close()
    }

    /** method for writing ArrayBuffer[(T, U)]
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      * @param path path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, path: String): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      val str: String = x.map(t => t.productIterator.toIndexedSeq.map(_.toString).mkString(",")).mkString("\n")
      bw.write(str)
      bw.close()
    }

    /** method for writing ArrayBuffer[(T, U)]
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      */
    def writeToCSV(fileName: String, rowNames: Option[Seq[String]], columnNames: Option[Seq[String]]): Unit = {
      SeqOfSeqExtensions.SeqOfSeqWriter(x.map(t => t.productIterator.toVector).transpose).writeToCSV(fileName, rowNames, columnNames)
    }

    /** method for writing ArrayBuffer[(T, U)]
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      * @param path path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, rowNames: Option[Seq[String]], columnNames: Option[Seq[String]], path: String): Unit = {
      SeqOfSeqExtensions.SeqOfSeqWriter(x.map(t => t.productIterator.toVector).transpose).writeToCSV(fileName, rowNames, columnNames, path)
    }
  }
}

/**
  * Extensions of scala.collection.mutable.Seq.
  * More general than specific implementations.
  */
object SeqOfSeqExtensions {

  /** Implicit class for writers of ArrayBuffer[(T, U)]
    * assumes that a mkString method exists for template parameters T and U.
    * If this is not the case, an error will likely appear.
    *
    * @param x ArrayBuffer of tuple of size 2 to write to column csv
    */
  implicit class SeqOfSeqWriter[T](x: scala.collection.Seq[scala.collection.Seq[T]]) {

    /** method for writing Seq[Seq[T]] to CSV a file.
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      */
    def writeToCSV(fileName: String): Unit = {
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(reduceSeqOfSeqToString(x, "").stripLineEnd)
      bw.close()
    }

    /** method for writing ArrayBuffer[(T, U)] with a second argument being the path specifying where to write the file.
      *
      * usage: ab.writeToCSV("test.csv", "/home/nicholas/")
      *
      * @param fileName the name of the file
      * @param path path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, path: String = ""): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(reduceSeqOfSeqToString(x, "").stripLineEnd)
      bw.close()
    }


    /** Writes the data including column and/or row names to CSV file. The column and row names are passed as [[Option]]
      * meaning they can be left as [[None]]. Both vectors (or other ordered collection) should not be set as None. The
      * method without names should be used instead.
      *
      * The column names vector must contain the name of the column containing the row names, if used. Hence if both
      * column and row names are used, then the column name vector will contain one element more.
      *
      * This function is a wrapper for the [[writeToCSVWithNames]] function allowing the usage of the same function name.
      *
      * @param fileName file to write to.
      * @param rowNames row names
      * @param columnNames column names
      */
    def writeToCSV(fileName: String, rowNames: Option[Seq[String]], columnNames: Option[Seq[String]]): Unit = writeToCSVWithNames(fileName, rowNames, columnNames)

    /** Writes the data including column and/or row names to CSV file. The column and row names are passed as [[Option]]
      * meaning they can be left as [[None]]. Both vectors (or other ordered collection) should not be set as None. The
      * method without names should be used instead.
      *
      * The column names vector must contain the name of the column containing the row names, if used. Hence if both
      * column and row names are used, then the column name vector will contain one element more.
      *
      * The final argument "path" is the path where to write th file.
      *
      * This function is a wrapper for the [[writeToCSVWithNames]] function allowing the usage of the same function name.
      *
      * @param fileName file to write to
      * @param rowNames row names
      * @param columnNames column names
      * @param path location of the file
      */
    def writeToCSV(fileName: String, rowNames: Option[Seq[String]], columnNames: Option[Seq[String]], path: String): Unit = writeToCSVWithNames(fileName, rowNames, columnNames, path)


    /** Process the row and column name vectors then writes the data to a file. Thos function is called by the wrappers
      * [[writeToCSV]] which mean the same function is always used. This is simply done for convenience.
      *
      * @param fileName file to write to
      * @param rowNames row names
      * @param columnNames column names
      * @param path location of the file
      */
    private def writeToCSVWithNames(fileName: String, rowNames: Option[Seq[String]], columnNames: Option[Seq[String]], path: String = ""): Unit = {

      // checks that the size of the data is consistent. If column names are used, there must always be a value for each column
      x.foreach(r => assert(r.size == x.head.size, "Inconsistent data, length of rows doesn't match length of first row ! first row length=" + x.head.size + ", buggy row lenght="+r.size))

      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      columnNames match {
        case Some(c) => { // column names are used
          // writes the column names to the file
          bw.write(c.mkString(",") + "\n")

          rowNames match {
            case Some(r) => { // row names and column names are used
              // since row and column names are used, checks if the number of column names is one more than the size of the data
              assert(r.size == x.head.size, "Number of row names is wrong ! data rows=" + x.head.size + ", row names rows=" + r.size)
              assert(c.size == x.size+1, "Number of column names is wrong ! data columns=" + x.size + ", column names=" + c.size)

              // Writes the data and row names to the file.
              bw.write(reduceSeqOfSeqToStringWithRowNames(x, r, "").stripLineEnd)
            }
            case None => { // column names are used but no row names
              // As only column names are passed as argument, the number of column names must equal the size of the data
              assert(c.size == x.size, "Number of column names is wrong ! data rows=" + x.size + ", column names=" + c.size)
              bw.write(reduceSeqOfSeqToString(x, "").stripLineEnd)
            }
          }
        }
        case None => { // no columm names
          rowNames match {
            case Some(r) => { // no column names but row names are used
              bw.write(reduceSeqOfSeqToStringWithRowNames(x, r, "").stripLineEnd)
            }
            case None => { // no row names and not column names, this case *should* never be used.
              println(" You shouldn't be using the CSV writer for row and/or column names without any column names !!\n Calling the correct method instead !!")
              writeToCSV(fileName, path)
            }
          }
        }
      }
      bw.close()
    }

    /** Convertes the first elements of Seq[Seq[T]] to Seq[String] for printing. If the data has different lengths,
      * then the default value is placed in the hole.
      *
      * @param x Seq[Seq[T]] for which the first elements must be converted to strings
      * @param default string to place if no value is present
      * @return sequence of Strings for the first elements of each rows
      */
    private def mapToStringWithDefault(x: scala.collection.Seq[scala.collection.Seq[T]], default: String = "nan"): Seq[String] = {
      for (v <- x) yield {
        if (v.isEmpty) {default}
        else {v.head.toString}
      }
    }


    private def tailWithEmptySeq(x: scala.collection.Seq[scala.collection.Seq[T]]): scala.collection.Seq[scala.collection.Seq[T]] = {
      for (v <- x) yield {
        if (v.isEmpty) {scala.collection.immutable.Vector()}
        else {v.tail}
      }
    }

    /** Writes the seq of seq as a string in a recursive way. The vectors contained in the
      * Seq are written as columns and the commas separate the values.
      *
      * @param seqOfSeq data to transform
      * @param str string accumulator
      * @return all the vectors as a string.
      */
    private def reduceSeqOfSeqToString(seqOfSeq: scala.collection.Seq[scala.collection.Seq[T]], str: String): String = {
      if (seqOfSeq.forall(_.isEmpty)) str
      else {
        reduceSeqOfSeqToString(tailWithEmptySeq(seqOfSeq), str + mapToStringWithDefault(seqOfSeq).mkString(",") + "\n")
      }
    }

    /** Creates a string out of an 2D Seq of Seq and adds a row name in front of each column.
      *
      * @param seqOfSeq data
      * @param rowNames Seq containing the row names
      * @param str accumulator
      * @return String formed from the data
      */
    private def reduceSeqOfSeqToStringWithRowNames(seqOfSeq: scala.collection.Seq[scala.collection.Seq[T]], rowNames: Seq[String], str: String): String = {
      if (seqOfSeq.forall(_.isEmpty)) str
      else {
        reduceSeqOfSeqToStringWithRowNames(tailWithEmptySeq(seqOfSeq), rowNames.tail, str + rowNames.head + "," + mapToStringWithDefault(seqOfSeq).mkString(",") + "\n")
      }
    }
  }
}