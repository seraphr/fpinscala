package jp.seraphr.fpinscala.datastructures

/**
 */
object Lists {
  def sequenceOpt[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Some(x) :: xs => sequenceOpt(xs).map(x :: _)
    case None :: xs    => None
    case _             => Some(List.empty)
  }
}
