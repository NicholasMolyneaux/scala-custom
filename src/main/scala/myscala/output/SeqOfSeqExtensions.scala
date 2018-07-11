package myscala.output

import java.io.{BufferedWriter, File, FileWriter}

import scala.util.{Failure, Success, Try}

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


      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))

      columnNames match {
        case Some(c) if Try(c.lengthCompare(x.length)).isSuccess || Try(c.lengthCompare(x.length+1)).isSuccess => { // column names are used
          // writes the column names to the file
          bw.write(c.mkString(",") + "\n")

          rowNames match {
            case Some(r) if Try(r.lengthCompare(x.head.size)).isSuccess && Try(c.lengthCompare(x.length+1)).isSuccess => { // row names and column names are used
              // Writes the data and row names to the file.
              bw.write(reduceSeqOfSeqToStringWithRowNames(x, r, "").stripLineEnd)
            }
            case None => { // column names are used but no row names
              // Writes the data without row names to the file.
              bw.write(reduceSeqOfSeqToString(x, "").stripLineEnd)
            }
          }
        }
        case None => { // no columm names
          rowNames match {
            case Some(r) if Try(r.lengthCompare(x.head.size)).isSuccess => { // no column names but row names are used
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