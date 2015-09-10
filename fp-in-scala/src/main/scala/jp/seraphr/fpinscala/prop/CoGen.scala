package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.state.RNG

/**
 */
trait CoGen[A] {
  protected def toLong(a: A): Long
  def cogen[B](a: A)(gen: Gen[B]): Gen[B] = Gen[B](RNG.reseed(toLong(a))(gen.sample), gen.exhaustive)
}

object CoGen {
  implicit object IntIsCogen extends CoGen[Int] {
    override protected def toLong(a: Int): Long = a.toLong
  }

  implicit object CharIsCogen extends CoGen[Char] {
    override protected def toLong(a: Char): Long = a.toLong
  }

  implicit object BooleanIsCogen extends CoGen[Boolean] {
    override protected def toLong(a: Boolean): Long = if (a) 1 else 0
  }

  private def iteratorCogen[A, B: CoGen](f: A => Iterator[B]) = new CoGen[A] {
    private val cogen = implicitly[CoGen[B]]

    override protected def toLong(a: A): Long = {
      f(a).map(cogen.toLong).reduceOption(_ ^ _).getOrElse(0)
    }
  }

  implicit val StringIsCogen: CoGen[String] = iteratorCogen(_.iterator)
}