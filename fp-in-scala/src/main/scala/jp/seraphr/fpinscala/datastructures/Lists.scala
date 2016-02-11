package jp.seraphr.fpinscala.datastructures

/**
 */
object Lists {
  def sequenceOpt[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Some(x) :: xs => sequenceOpt(xs).map(x :: _)
    case None :: xs    => None
    case _             => Some(List.empty)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil    => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil    => z
    case h :: t => f(h, foldRight(t, z)(f))
  }
}
