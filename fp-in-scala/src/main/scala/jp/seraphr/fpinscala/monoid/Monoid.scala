package jp.seraphr.fpinscala.monoid

import jp.seraphr.fpinscala.parallelism.Nonblocking

/**
 */
trait Monoid[A] {
  def op(l: A, r: A): A
  def zero: A
}

object Monoid {
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(l: Int, r: Int): Int = l + r
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(l: Int, r: Int): Int = l * r
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(l: Boolean, r: Boolean): Boolean = l || r
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(l: Boolean, r: Boolean): Boolean = l && r
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(l: Option[A], r: Option[A]): Option[A] = l orElse r
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(l: A => A, r: A => A): A => A = l andThen r
    override def zero: A => A = identity[A]
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case IndexedSeq()  => m.zero
    case IndexedSeq(v) => f(v) // = m.op(m.zero, f(v))
    case _ =>
      val (ls, rs) = as.splitAt(as.length / 2)
      m.op(foldMapV(ls, m)(f), foldMapV(rs, m)(f))
  }
  def foldMapViaFoldLeft[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((acc, e) => m.op(acc, f(e)))
  def foldMapViaFoldRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((e, acc) => m.op(f(e), acc))
  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val tMonoid = new Monoid[B => B] {
      override def op(l: B => B, r: B => B): B => B = l andThen r
      override def zero: B => B = identity[B]
    }

    foldMap(as, tMonoid)(a => b => f(b, a))(z)
  }
  def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val tMonoid = new Monoid[B => B] {
      override def op(l: B => B, r: B => B): B => B = l compose r
      override def zero: B => B = identity[B]
    }

    foldMap(as, tMonoid)(a => b => f(a, b))(z)
  }

  import Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      override def op(l: Par[A], r: Par[A]): Par[A] = Nonblocking.map2(l, r)(m.op)
      override def zero: Par[A] = Nonblocking.unit(m.zero)
    }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)
    val fm = asyncF(f)
    fork(foldMapV(v, pm)(fm))
  }
}
