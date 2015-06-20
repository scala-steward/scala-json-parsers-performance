package jsonperf

import scalaz.{Failure, Success}

abstract class JsonTest[A](implicit ev: scala.reflect.Manifest[A]) extends Serializable {

  def json: String
  def newA: A
  def clazz: Class[A]
  def sphereFromJson: io.sphere.json.FromJSON[A]
  def playRead: play.api.libs.json.Reads[A]
  def sprayRead: spray.json.JsonReader[A]
  def argonautDecodeJson: argonaut.DecodeJson[A]
  def checkResult(result: A): Unit


  type Parsing = JsonParsing[A]

  val noParsing: Parsing = new Parsing {
    override def apply(json: String): A = newA
    override def toString(): String = "noParsing"
  }
  val jacksonParsing: Parsing = new Parsing {
    override def apply(json: String): A = Jackson.mapper.readValue(json, clazz)
    override def toString(): String = "jackson"
  }

  val json4sNative: Parsing = new Parsing {
    override def apply(json: String): A = {
      import org.json4s._
      import org.json4s.native.JsonMethods._
      implicit val formats = DefaultFormats
      parse(json).extract[A]
    }
    override def toString(): String = "json4sNative"
  }

  val json4sJackson: Parsing = new Parsing {
    override def apply(json: String): A = {
      import org.json4s._
      import org.json4s.jackson.JsonMethods._
      implicit val formats = DefaultFormats
      parse(json).extract[A]
    }
    override def toString(): String = "json4sJackson"
  }

  val sphereJson: Parsing = new Parsing {
    override def apply(json: String): A = {
      io.sphere.json.getFromJSON(json)(sphereFromJson)
    }
    override def toString(): String = "sphereJson"
  }

  val playJson: Parsing = new Parsing {
    override def apply(json: String): A = {
      import play.api.libs.json.Json
      Json.parse(json).as[A](playRead)
    }
    override def toString(): String = "playJson"
  }

  val sprayJson: Parsing = new Parsing {
    override def apply(json: String): A = {
      import spray.json._
      JsonParser(json).convertTo[A](sprayRead)
    }
    override def toString(): String = "sprayJson"
  }

  val argonautJson: Parsing = new Parsing {
    override def apply(json: String): A = {
      argonaut.Parse.decodeValidation[A](json)(argonautDecodeJson) match {
        case Success(a) ⇒ a
        case Failure(t) ⇒ throw new Exception(t)
      }
    }
    override def toString(): String = "argonaut"
  }
}
