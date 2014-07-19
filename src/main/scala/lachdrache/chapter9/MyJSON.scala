package lachdrache.chapter9

import scala.language.higherKinds

trait MyJSON

object MyJSON {
  case object JNull extends MyJSON
  case class JNumber(get: Double) extends MyJSON
  case class JString(get: String) extends MyJSON
  case class JBool(get: Boolean) extends MyJSON
  case class JArray(get: IndexedSeq[MyJSON]) extends MyJSON
  case class JObject(get: Map[String, MyJSON]) extends MyJSON

  /**
   * No leading and trailing whitespace supported
   */
  def jsonParser[ParseError, Parser[+ _]](P: Parsers[Parser]): Parser[MyJSON] = {
    import P._

    def jNumber: Parser[JNumber] =
      P.number map JNumber

    def jBoolean: Parser[JBool] =
      ("true" | "false") map { s =>
        JBool(s == "true")
      }

    def jString: Parser[JString] =
      stringLiteral map (JString(_))

    def leadingSeparatorWithJson: Parser[MyJSON] =
      ("," *** json) map { case (_, json) => json }

    def manyLeadingSeparatorWithJson: Parser[List[MyJSON]] =
      many(leadingSeparatorWithJson)

    def jArrayWithAtLeastOneElement: Parser[JArray] =
      ("[" *** json ***  manyLeadingSeparatorWithJson *** "]") map { case (((_, h), t), _) =>
        JArray((h :: t).toIndexedSeq)
      }

    def jArrayWithNoElements: Parser[JArray] =
      ("[" *** "]") map { (_) => JArray(IndexedSeq()) }

    def jArray: Parser[JArray] =
      jArrayWithAtLeastOneElement | jArrayWithNoElements

    def keyValue: Parser[(String, MyJSON)] =
      stringLiteral *** ":" *** json map { case ((k, _), v) =>
        (k, v)
      }

    def keyValueWithSeparator: Parser[(String,MyJSON)] =
      "," *** keyValue map { case (_, kv) => kv }

    def keyValues: Parser[Map[String, MyJSON]] =
      many(keyValueWithSeparator) map { l => l.toMap}

    def jObjectWithAtLeastOneElement: Parser[JObject] =
      "{" *** keyValue *** many(keyValueWithSeparator) *** "}" map { case (((_, kv), kvs), _) =>
        JObject((kv :: kvs).toMap)
      }

    def jObjectWithNoElements: Parser[JObject] =
      "{" *** "}" map { _ => JObject(Map.empty) }

    def jObject: Parser[JObject] =
      jObjectWithAtLeastOneElement | jObjectWithNoElements

    def json: Parser[MyJSON] = jBoolean/* | jNumber | jString | jArray | jObject*/

    json

    // TODO Create new combinator repetition with separator to simplify jArray and jObject definition
    // TODO The literal null is missing
  }
}

