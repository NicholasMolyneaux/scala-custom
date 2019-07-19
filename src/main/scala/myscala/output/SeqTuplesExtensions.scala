package myscala.output

import java.io.{BufferedWriter, File, FileWriter}

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
    def writeToCSV(fileName: String, rowNames: Option[scala.collection.immutable.Seq[String]], columnNames: Option[scala.collection.immutable.Seq[String]]): Unit = {
      SeqOfSeqExtensions.SeqOfSeqWriter(x.map(t => t.productIterator.toVector).transpose).writeToCSV(fileName, rowNames, columnNames)
    }

    /** method for writing ArrayBuffer[(T, U)]
      *
      * usage: ab.writeToCSV("test.csv")
      *
      * @param fileName the name of the file
      * @param path path with trailing /, default is empty ""
      */
    def writeToCSV(fileName: String, rowNames: Option[scala.collection.immutable.Seq[String]], columnNames: Option[scala.collection.immutable.Seq[String]], path: String): Unit = {
      SeqOfSeqExtensions.SeqOfSeqWriter(x.map(t => t.productIterator.toVector).transpose).writeToCSV(fileName, rowNames, columnNames, path)
    }
  }
}