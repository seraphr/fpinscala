package jp.seraphr.fpinscala.parser

import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.prop.Prop

import language.higherKinds
import language.implicitConversions
import scala.util.matching.Regex

/**
 */
trait Parsers[Parser[+_]] {
  self =>
  type ParseError

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def whitespace = regex("\\s*".r)
  def eof: Parser[String] = regex("\\z".r)

  implicit class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B) = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C) = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def many = self.many(p)
    def many1 = self.many1(p)
    def sep(sep: Parser[Any]) = self.sep(p, sep)
    def slice = self.slice(p)
    def |[B >: A](p2: => Parser[B]) = self.or(p, p2)
    def orEmpty = self.orEmpty(p)
    def product[B](p2: => Parser[B]) = self.product(p, p2)
    def **[B](p2: => Parser[B]) = self.product(p, p2)
    def *>[B](p2: => Parser[B]) = self.productRight(p, p2)
    def <*[B](p2: => Parser[B]) = self.productLeft(p, p2)

    def ignoreWhitespace = self.ignoreWhitespace(p)

    def left[L, R](implicit ev: A <:< (L, R)): Parser[L] = p.map(a => ev(a)._1)
    def right[L, R](implicit ev: A <:< (L, R)): Parser[R] = p.map(a => ev(a)._2)
    def center[L, C, R](implicit ev: A =:= ((L, C), R)): Parser[C] = p.map(a => ev(a)._1._2)
  }

  object Laws {
    object Laws {

      import Prop.{ run => _, _ }
      import Gen._

      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop = {
        forAll(in) { s =>
          val tProductResult = run(product(p1, p2).slice)(s)
          val tSeparatelyResult =
            for {
              tP1Result <- run(p1.slice)(s).right
              tP2Result <- run(p1.slice)(s.substring(tP1Result.length)).right
            } yield tP1Result + tP2Result

          tProductResult == tSeparatelyResult
        }
      }
    }
  }

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List.empty)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case n if n <= 0 => succeed(List.empty)
    case n           => map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def sep[A](p: Parser[A], sep: Parser[Any]): Parser[List[A]] = (p ** (sep *> p).many).map { case (v, vs) => v :: vs }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def orEmpty[A](p: Parser[A]): Parser[Option[A]] = p.map(Option(_)) | succeed(Option.empty)
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(f andThen succeed)
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p1
      b <- p2
    } yield f(a, b)

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)] =
    for {
      ar <- a
      br <- b
    } yield (ar, br)

  def productRight[A, B](a: Parser[A], b: => Parser[B]): Parser[B] = product(a, b).right
  def productLeft[A, B](a: Parser[A], b: => Parser[B]): Parser[A] = product(a, b).left

  def contextSensitiveParser: Parser[List[Char]] = "\\d".r.flatMap(n => listOfN(n.toInt, char('a')))

  /**
   * ignore が前後にn個つくのを無視する
   * @param ignore
   * @param p
   * @tparam A
   * @return
   */
  def ignore[A](ignore: Parser[Any], p: Parser[A]): Parser[A] = ignore.many *> p <* ignore.many
  def ignoreWhitespace[A](p: Parser[A]) = ignore(whitespace, p)
}
