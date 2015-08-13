package jp.seraphr.fpinscala.datastructures

/**
 */
object Options {
  def sequenceStream[A](aStreamOpt: Option[Stream[A]]): Stream[Option[A]] = aStreamOpt match {
    case Some(s) => s.map(Some(_))
    case None    => Stream.empty
  }
}
