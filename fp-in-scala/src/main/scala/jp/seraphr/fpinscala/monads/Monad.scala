package jp.seraphr.fpinscala.monads

import jp.seraphr.fpinscala.datastructures.Options
import jp.seraphr.fpinscala.laziness.Streams
import jp.seraphr.fpinscala.parallelism.{ Par, ParApi }
import jp.seraphr.fpinscala.parser.Parsers
import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.state.State
import jp.seraphr.fpinscala.utils.Equal

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

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = la match {
    case Nil     => unit(Nil)
    case a :: as => map2(f(a), traverse(as)(f))(_ :: _)
  }
  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity)

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))
}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  private def genParMonad(aApi: ParApi): Monad[aApi.Par] = new Monad[aApi.Par] {
    import aApi.ParOpts

    override def flatMap[A, B](ma: aApi.Par[A])(f: (A) => aApi.Par[B]): aApi.Par[B] = ma.flatMap(f)
    override def unit[A](a: A): aApi.Par[A] = aApi.unit(a)
  }

  val parMonad = genParMonad(Par)
  def parserMonad[P[+_]](aApi: Parsers[P]) = new Monad[P] {
    override def unit[A](a: A): P[A] = aApi.succeed(a)
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = aApi.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
  }

  def stateMonad[S]: Monad[({ type M[X] = State[S, X] })#M] = new Monad[({ type M[X] = State[S, X] })#M] {
    override def unit[A](a: A): State[S, A] = State.unit(a)
    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }
}

object MonadLaws {
  import Gen._
  import jp.seraphr.fpinscala.prop.Prop._

  def monadLaws[M[_], A, B, C](m: Monad[M], aGenA: Gen[A], aGenF: Gen[A => M[B]], aGenG: Gen[B => M[C]])(implicit eqA: Equal[M[A]], eqB: Equal[M[B]], eqC: Equal[M[C]]): Prop = {
    implicit class MonadOpt[X](l: M[X]) {
      def flatMap[Y](f: X => M[Y]): M[Y] = m.flatMap(l)(f)
      def ===(r: M[X])(implicit eq: Equal[M[X]]) = eq.eq(l, r)
    }

    val tLeftIdentityLaw = forAll(tuple(aGenA, aGenF)) {
      case (a, f) =>
        m.unit(a).flatMap(f) === f(a)
    }

    val tRightIdentityLaw = forAll(aGenA.map(m.unit)) {
      ma =>
        ma.flatMap(m.unit) === ma
    }

    val tAssociativeLaw = forAll(tuple(aGenA.map(m.unit), tuple(aGenF, aGenG))) {
      case (ma, (f, g)) =>
        ma.flatMap(f).flatMap(g) === ma.flatMap(a => f(a).flatMap(g))
    }

    tLeftIdentityLaw && tRightIdentityLaw && tAssociativeLaw
  }
}
