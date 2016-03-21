package jp.seraphr.fpinscala.monads

import jp.seraphr.fpinscala.prop.Gen

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma) { a => map(mb) { b => f(a, b) } }

}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma.flatMap(f)
  }
}
