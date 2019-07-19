package myscala.input

import play.api.libs.json._
import scala.io.BufferedSource

/**
  * Reads JSON files into a generic container. The values are stored as strings which need to be parsed
  * to the desired type.
  */
object ReadJson {

  /**
    * Container with the field name form the JSON file and the list of items stored as strings. They can easily be
    * converted to doubles or ints with scala's methods.
    * @param str fieldname
    * @param v list of strings representing the data
    */
  case class Container(str: String, v: Vector[String])



  /**
    * implicit reader for the play framework to convert the strings to JSON format. Reads a JSON file looking like
    * {
    *  "scalatest" : [ "1.1", "2.01", "3.0005", "4.99", "5", "6", "7", "8", "9", "10.1" ]
    * }
    * and fills the [[Container]] class with the data.
    */
  implicit val ContainerReads: Reads[Vector[Container]] =
    new Reads[Vector[Container]] {
      override def reads(json: JsValue): JsResult[Vector[Container]] = json match {
        case j: JsObject => JsSuccess(j.fields.map(i => Container(i._1, i._2.validate[Vector[String]].get.toVector)).toVector)
        case _ => JsError("Invalid JSON type")
      }
    }

  /**
    * Takes the file and parses the file into the [[Container]] class.
    * @param str file to read
    * @return [[Container]] storing the data
    */
  def readVector(str: String): Option[Vector[Container]] = {
    val sourceTest: BufferedSource = scala.io.Source.fromFile(str)
    val inputTest: JsValue = Json.parse(try sourceTest.mkString finally sourceTest.close)
    inputTest.asOpt[Vector[Container]]
  }


  /**
    * Container which stores a sequence of sub containers. Similarly to the other subcontainer, the fieldname
    * is salso stored.
    * @param str fieldname from JSON
    * @param v sequence of containers stoing the data using the [[Container]] case class.
    */
  case class ContainerMultipleVector(str: String, v: Vector[Container])

  /**
    * implicit reader for the play framework to convert the strings to JSON format. Reads a JSON file looking like
    *{
    * "scalatest" : {
    *   "a" : [ "1.5", "2.0", "3.0" ],
    *   "b" : [ "6.0", "7.0", "8.99", "9.0", "10.0" ],
    *   "c" : [ "4.0", "15.0" ]
    *  }
    *}
    * and fills the [[ContainerMultipleVector]] class with the data.
    */
  implicit val Container2Reads: Reads[ContainerMultipleVector] =
    new Reads[ContainerMultipleVector] {
      override def reads(json: JsValue): JsResult[ContainerMultipleVector] = json match {
        case j: JsObject =>
          JsSuccess(ContainerMultipleVector(j.fields.head._1, j.fields.head._2.validate[Vector[Container]] match {
            case k: JsSuccess[Vector[Container]] => k.get
            case _ => throw new Exception("Invalid JSON type a")
          }))
        case _ => JsError("Invalid JSON type")
      }
    }

  /**
    * Takes the file and parses the file into the [[ContainerMultipleVector]] class.
    *
    * @param str file to read
    * @return [[ContainerMultipleVector]] storing the data
    */
  def readMultipleVectors(str: String): Option[ContainerMultipleVector] = {
    val sourceTest: BufferedSource = scala.io.Source.fromFile(str)
    val inputTest: JsValue = Json.parse(try sourceTest.mkString finally sourceTest.close)
    inputTest.asOpt[ContainerMultipleVector]
  }
}

