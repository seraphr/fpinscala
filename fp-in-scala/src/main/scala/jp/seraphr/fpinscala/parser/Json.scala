package jp.seraphr.fpinscala.parser

import language.higherKinds

trait Json

/**
 */
object Json {
  case object JNull extends Json
  case class JNumber(get: Double) extends Json
  case class JString(get: String) extends Json
  case class JBool(get: Boolean) extends Json
  case class JArray(get: IndexedSeq[Json]) extends Json
  case class JObject(get: Map[String, Json]) extends Json

  def parser[Parser[+_]](aParserApi: Parsers[Parser]): Parser[Json] = {
    type Error = ParseError
    import aParserApi._

    lazy val fields = field.sep(",").map(_.toMap)
    lazy val field = ((stringValue <* ":".ignoreWhitespace) ** value).ignoreWhitespace
    lazy val arrayElements = value.sep(",")
    lazy val digit = regex("\\d".r)
    lazy val digit19 = regex("[1-9]".r)
    lazy val digit0 = string("0")
    lazy val digits = digit.many1
    lazy val numberBeforeDot = ("-".orEmpty ** ((digit19 ** digits.orEmpty).slice | digit0)).slice.scope("num before dot")
    lazy val numberAfterDot = (digits ** (("e" | "E") ** ("+" | "-").orEmpty ** digits.orEmpty).orEmpty).slice.scope("num after dot")
    // string はちゃんと定義するのめんどくさいので適当
    lazy val stringValue = ("\"" *> "[a-zA-Z0-9 ]*".r <* "\"")

    // JValueParsers
    lazy val objectParser = ("{".ignoreWhitespace *> fields <* "}".ignoreWhitespace).map(s => JObject(s)).scope("obj")
    lazy val array = ("[" *> arrayElements <* "]").map(vs => JArray(vs.toIndexedSeq))
    lazy val value: Parser[Json] = (stringParser | number | objectParser | array | bool | nullParser).ignoreWhitespace
    lazy val stringParser = stringValue.map(JString)
    lazy val number = (numberBeforeDot ** ("." ** numberAfterDot).orEmpty).slice.map(n => JNumber(n.toDouble)).scope("number")
    lazy val nullParser = string("null").map(_ => JNull)
    lazy val bool = ("true" | "false").map(b => JBool(b.toBoolean)).scope("bool")

    (objectParser | array) <* eof
  }
}
