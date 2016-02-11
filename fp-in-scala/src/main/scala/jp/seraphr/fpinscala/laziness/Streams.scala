package jp.seraphr.fpinscala.laziness

/**
 */
object Streams {
  def foldRight[A, B](s: Stream[A])(z: => B)(f: (A, => B) => B): B = s match {
    case x #:: xs => f(x, foldRight(xs)(z)(f))
    case _ => z
  }

  def foldLeft[A, B](as: Stream[A])(z: => B)(f: (=> B, A) => B): B = as match {
    case x #:: xs => foldLeft(xs)(f(z, x))(f)
    case _ => z
  }



  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s) match {
    case Some((a, s1)) => a #:: unfold(s1)(f)
    case _             => Stream.empty[A]
  }

  def fibs: Stream[Int] = 0 #:: 1 #:: unfold((0, 1)) {
    case (n0, n1) =>
      val n2 = n0 + n1
      Option((n2, (n1, n2)))
  }

  def from(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))
  def constant[A](n: A): Stream[A] = unfold(n)(a => Some((a, a)))
  def ones: Stream[Int] = constant(1)
  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case x #:: xs => Some((f(x), xs))
    case _        => None
  }

  def take[A](s: Stream[A])(n: Int): Stream[A] = unfold((n, s)) {
    case (0, _)        => None
    case (n, x #:: xs) => Some((x, (n - 1, xs)))
    case _             => None
  }

  def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = unfold(s) {
    case x #:: xs if f(x) => Some((x, xs))
    case _                => None
  }

  def zipWith[A, B, C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((s1, s2)) {
    case (x #:: xs, y #:: ys) => Some((f(x, y), (xs, ys)))
    case _                    => None
  }

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((s1, s2)) {
    case (x #:: xs, y #:: ys) => Some(((Some(x), Some(y)), (xs, ys)))
    case (_, y #:: ys)        => Some(((None, Some(y)), (Stream.empty, ys)))
    case (x #:: xs, _)        => Some(((Some(x), None), (xs, Stream.empty)))
    case _                    => None
  }

  def startsWith[A](s: Stream[A], start: Stream[A]): Boolean = zipAll(s, start).forall {
    case (l, r) => l.isDefined && (l == r || r.isEmpty)
  }

  def scanRight[A, B](s: Stream[A])(z: => B)(f: (A, => B) => B): Stream[B] = s match {
    case x #:: xs =>
      lazy val tTails = scanRight(xs)(z)(f)
      f(x, tTails.head) #:: tTails
    case _ => Stream(z)
  }

  def tails[A](s: Stream[A]): Stream[Stream[A]] = scanRight(s)(Stream.empty[A])(_ #:: _)
}
