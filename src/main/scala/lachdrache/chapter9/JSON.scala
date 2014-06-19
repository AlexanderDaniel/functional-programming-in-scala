package lachdrache.chapter9

import scala.language.higherKinds

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParseError, Parser[+ _]](P: Parsers[ParseError,Parser]): Parser[JSON] = {
    import P._

    def jNumber: Parser[JNumber] = P.number map JNumber
    def jBoolean: Parser[JBool] = ("true" | "false") map { s =>
      JBool(s == "true")
    }
    def jString: Parser[JString] = stringLiteral map (JString(_))
    def jArray: Parser[JArray] = "[" ** many(json) ** "]" map { case ((_, arr), _) =>
      JArray(arr.toIndexedSeq)
    }
    def keyValue: Parser[(String, JSON)] =
      stringLiteral ** ":" ** json map { case ((k, _), v) =>
        (k, v)
      }
    def keyValues: Parser[Map[String, JSON]] =
      many(keyValue) map { l => l.toMap }
    def jObject: Parser[JObject] = "{" ** keyValues ** "}" map { case ((_, kvs), _) =>
      JObject(kvs)
    }
    // TODO handle whitespace

    def json: Parser[JSON] = jNumber | jString | jBoolean | jArray | jObject

    json
  }
}

