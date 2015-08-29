package jp.seraphr.fpinscala.parser

import scala.util.matching.Regex
import language.implicitConversions

/**
 * parse成功時の返り値
 *
 * @param v 成功した結果として生成された値
 * @param s vの元になった、parserにマッチした入力文字列
 * @tparam A
 */
case class SuccessResult[+A](v: A, s: String)
case class MyParser[+A](parse: (String, Location) => Either[ParseError, SuccessResult[A]])

object MyParsers extends Parsers[MyParser] {
  private def newError(aLoc: Location, aMessage: String) =
    ParseError((aLoc, aMessage) :: Nil)

  private def newLeftError(aLoc: Location, aMessage: String) =
    Left(newError(aLoc, aMessage))

  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = p.parse(input, Location(input)).right.map(_.v)

  override def succeed[A](a: A): MyParser[A] = MyParser { (tInput, tLocation) =>
    Right(SuccessResult(a, ""))
  }

  override implicit def string(s: String): MyParser[String] = MyParser { (tInput, tLocation) =>
    if (tInput.startsWith(s))
      Right(SuccessResult(s, s))
    else
      newLeftError(tLocation, s""""${s}"を予想していましたが"${tInput.take(s.length)}"が見つかりました。""") //; throw new Exception()
  }

  override implicit def regex(r: Regex): MyParser[String] = MyParser { (tInput, tLocation) =>
    r.findPrefixOf(tInput) match {
      case Some(s) => Right(SuccessResult(s, s))
      case None    => newLeftError(tLocation, s"""正規表現"${r.regex}"にマッチしませんでした。 "${tInput.take(r.regex.length)}"""")
    }
  }

  override def flatMap[A, B](a: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = MyParser { (tInput, tLocation) =>
    a.parse(tInput, tLocation) match {
      case Right(SuccessResult(v, s)) =>
        val tOffset = s.length
        // 最初のparserで1文字以上読み込んでいる時のみ、強制コミットする
        val tP2 = if (tOffset == 0) f(v) else commit(f(v))
        tP2.parse(tInput.substring(tOffset), tLocation.copy(offset = tLocation.offset + tOffset)).right.map(r2 => r2.copy(s = s + r2.s))
      case Left(e) => Left(e)
    }
  }
  override def slice[A](p: MyParser[A]): MyParser[String] = MyParser { (tInput, tLocation) =>
    p.parse(tInput, tLocation).right.map(v => SuccessResult(v.s, v.s))
  }

  override def label[A](aLabel: String)(p: MyParser[A]): MyParser[A] = MyParser { (tInput, tLocation) =>
    p.parse(tInput, tLocation).left.map(v => newError(tLocation, aLabel).copy(isCommitted = v.isCommitted))
  }
  override def scope[A](aLabel: String)(p: MyParser[A]): MyParser[A] = MyParser { (tInput, tLocation) =>
    p.parse(tInput, tLocation).left.map(v => ParseError((tLocation, aLabel) :: v.stack, v.isCommitted))
  }

  private def mapError[A](p: MyParser[A])(f: ParseError => ParseError): MyParser[A] = MyParser { (tInput, tLocation) =>
    p.parse(tInput, tLocation).left.map(f)
  }

  override def attempt[A](p: MyParser[A]): MyParser[A] = mapError(p)(_.copy(isCommitted = false))

  override def commit[A](p: MyParser[A]): MyParser[A] = mapError(p)(_.copy(isCommitted = true))

  override def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = MyParser { (tInput, tLocation) =>
    p1.parse(tInput, tLocation).left.flatMap {
      case ParseError(_, false) => p2.parse(tInput, tLocation)
      case other                => Left(other)
    }
  }
}
