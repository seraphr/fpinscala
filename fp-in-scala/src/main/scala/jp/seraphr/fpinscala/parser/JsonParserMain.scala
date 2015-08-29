package jp.seraphr.fpinscala.parser

/**
 */
object JsonParserMain extends App {
  val tParser = Json.parser(MyParsers)
  val tJson =
    """{
      |  "s" : "string",
      |  "b1" : true,
      |  "b2" : false,
      |  "n1" : 0.123,
      |  "n2" : 8.38e9,
      |  "a" : ["a", "r", "r", "a", "y"]
      |}
    """.stripMargin
  println(MyParsers.run(tParser)(tJson))
}

