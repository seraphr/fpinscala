package jp.seraphr.fpinscala.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s0 =>
    val (a, s1) = run(s0)
    (f(a), s1)
  }

  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = State { s0 =>
    val (a, s1) = this.run(s0)
    val (b, s2) = that.run(s1)
    (f(a, b), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s0 =>
    val (a, s1) = run(s0)
    f(a).run(s1)
  }
}

object State {
  def unit[S, A](a: A) = State[S, A](s => (a, s))

  def sequence[S, A](aList: List[State[S, A]]): State[S, List[A]] = aList match {
    case x :: xs => x.map2(sequence(xs))(_ :: _)
    case _ => unit[S, List[A]](Nil)
  }
}