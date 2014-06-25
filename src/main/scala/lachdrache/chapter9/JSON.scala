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

  /**
   * No leading and trailing whitespace supported
   */
  def jsonParser[ParseError, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def jNumber: Parser[JNumber] =
      P.number map JNumber

    def jBoolean: Parser[JBool] =
      ("true" | "false") map { s =>
        JBool(s == "true")
      }

    def jString: Parser[JString] =
      stringLiteral map (JString(_))

    def leadingSeparatorWithJson: Parser[JSON] =
      ("," *** json) map { case (_, json) => json }

    def manyLeadingSeparatorWithJson: Parser[List[JSON]] =
      many(leadingSeparatorWithJson)

    def jArrayWithAtLeastOneElement: Parser[JArray] =
      ("[" *** json ***  manyLeadingSeparatorWithJson *** "]") map { case (((_, h), t), _) =>
        JArray((h :: t).toIndexedSeq)
      }

    def jArrayWithNoElements: Parser[JArray] =
      ("[" *** "]") map { (_) => JArray(IndexedSeq()) }

    def jArray: Parser[JArray] =
      jArrayWithAtLeastOneElement | jArrayWithNoElements

    def keyValue: Parser[(String, JSON)] =
      stringLiteral *** ":" *** json map { case ((k, _), v) =>
        (k, v)
      }

    def keyValueWithSeparator: Parser[(String,JSON)] =
      "," *** keyValue map { case (_, kv) => kv }

    def keyValues: Parser[Map[String, JSON]] =
      many(keyValueWithSeparator) map { l => l.toMap}

    def jObjectWithAtLeastOneElement: Parser[JObject] =
      "{" *** keyValue *** many(keyValueWithSeparator) *** "}" map { case (((_, kv), kvs), _) =>
        JObject((kv :: kvs).toMap)
      }

    def jObjectWithNoElements: Parser[JObject] =
      "{" *** "}" map { _ => JObject(Map.empty) }

    def jObject: Parser[JObject] =
      jObjectWithAtLeastOneElement | jObjectWithNoElements

    def json: Parser[JSON] = jNumber | jString | jBoolean | jArray | jObject

    json

    // TODO Create new combinator repetition with separator to simplify jArray and jObject definition
    // TODO The literal null is missing
  }
}

