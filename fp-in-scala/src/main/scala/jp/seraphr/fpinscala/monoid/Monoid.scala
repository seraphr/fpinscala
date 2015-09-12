package jp.seraphr.fpinscala.monoid

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
}
