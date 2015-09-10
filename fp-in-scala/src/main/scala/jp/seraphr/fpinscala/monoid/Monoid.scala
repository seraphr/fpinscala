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
}
