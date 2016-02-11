package jp.seraphr.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def foldLeft[A, B](as: Tree[A], z: B)(f: (B, A) => B): B = as match {
    case Leaf(a)      => f(z, a)
    case Branch(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
  }

  def foldRight[A, B](as: Tree[A], z: B)(f: (A, B) => B): B = as match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => foldRight(l, foldRight(r, z)(f))(f)
  }

  def fold[A, B](t: Tree[A])(map: A => B, construct: (B, B) => B): B = t match {
    case Leaf(v)      => map(v)
    case Branch(l, r) => construct(fold(l)(map, construct), fold(r)(map, construct))
  }
}