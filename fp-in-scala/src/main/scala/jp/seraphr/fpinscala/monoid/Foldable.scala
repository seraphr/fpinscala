package jp.seraphr.fpinscala.monoid

import jp.seraphr.fpinscala.datastructures.{ Tree, Lists }
import jp.seraphr.fpinscala.laziness.Streams
import language.higherKinds

/**
 */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldMap(as)(identity)(m)
}

object Foldable {
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = Lists.foldRight(as, z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = Lists.foldLeft(as, z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as match {
      case IndexedSeq() => z
      case _            => f(as.head, foldRight(as.tail)(f(as.head, z))(f))
    }

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as match {
      case IndexedSeq() => z
      case _            => foldLeft(as.tail)(z)(f)
    }
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = Streams.foldRight(as)(z)((a, b) => f(a, b))
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = Streams.foldLeft(as)(z)((b, a) => f(b, a))
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  }
}

