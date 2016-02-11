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

  sealed trait IsSorted
  object IsSorted {
    case class Sorted(vs: List[Int]) extends IsSorted
    case object NotSorted extends IsSorted
    case object Init extends IsSorted
  }

  val intSorted: Monoid[IsSorted] = new Monoid[IsSorted] {
    import IsSorted._
    override def op(l: IsSorted, r: IsSorted): IsSorted = (l, r) match {
      case (Init, _)                                    => r
      case (_, Init)                                    => l
      case (NotSorted, _)                               => NotSorted
      case (_, NotSorted)                               => NotSorted
      case (Sorted(lv), Sorted(rv)) if lv.max <= rv.min => Sorted(List(lv.min, rv.max))
      case _                                            => NotSorted
    }
    override def zero: IsSorted = Init
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

  def isSorted(as: IndexedSeq[Int]): Boolean = foldMapV(as, intSorted)(e => IsSorted.Sorted(List(e))) != IsSorted.NotSorted

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

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(l: (A, B), r: (A, B)): (A, B) = (l, r) match {
      case ((ll, lr), (rl, rr)) => (A.op(ll, rl), B.op(lr, rr))
    }
    override val zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(l: A => B, r: A => B): A => B = a => B.op(l(a), r(a))
    override def zero: A => B = _ => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()
    override def op(l: Map[K, V], r: Map[K, V]): Map[K, V] = {
      (l.keySet ++ r.keySet).foldLeft(zero) { (acc, k) =>
        val tLValue = l.getOrElse(k, V.zero)
        val tRValue = r.getOrElse(k, V.zero)
        acc.updated(k, V.op(tLValue, tRValue))
      }
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val tBagMonoid = mapMergeMonoid[A, Int](intAddition)
    foldMapV(as, tBagMonoid)(a => Map(a -> 1))
  }
}
