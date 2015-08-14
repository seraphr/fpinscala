package jp.seraphr.fpinscala.parser

import language.higherKinds
import language.implicitConversions

/**
  */
trait Parsers[Parser[+_]] { self =>
  type ParseError

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  implicit def string(s: String): Parser[String]
  implicit class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B) = self.map(p)(f)
    def many = self.many(p)
    def slice = self.slice(p)
    def |(p2: => Parser[A]) = self.or(p, p2)
    def product(p2: => Parser[A]) = self.product(p, p2)
    def **(p2: => Parser[A]) = self.product(p, p2)
  }

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def slice[A](p: Parser[A]): Parser[String]
  def many[A](p: Parser[A]): Parser[List[A]]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
}
