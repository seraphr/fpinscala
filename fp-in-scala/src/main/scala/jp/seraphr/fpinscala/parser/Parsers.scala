package jp.seraphr.fpinscala.parser

import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.prop.Prop

import language.higherKinds
import language.implicitConversions

/**
 */
trait Parsers[Parser[+_]] { self =>
  type ParseError

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  implicit def string(s: String): Parser[String]
  implicit class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B) = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C) = self.map2(p, p2)(f)
    def many = self.many(p)
    def many1 = self.many1(p)
    def slice = self.slice(p)
    def |(p2: => Parser[A]) = self.or(p, p2)
    def product(p2: => Parser[A]) = self.product(p, p2)
    def **(p2: => Parser[A]) = self.product(p, p2)
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

  def slice[A](p: Parser[A]): Parser[String]
  def many[A](p: Parser[A]): Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map { case (a, b) => f(a, b) }

  def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)]
}
