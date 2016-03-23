package jp.seraphr.fpinscala.utils

import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.prop.Prop._
import jp.seraphr.fpinscala.state.RNG

trait Equal[A] {
  def eq(l: A, r: A): Boolean
}

trait WeakEqualsInstance {
  implicit def DefaultEq[A]: Equal[A] = new Equal[A] {
    override def eq(l: A, r: A): Boolean = l == r
  }
}

object Equal extends WeakEqualsInstance {
  implicit def EndoEq[A: Gen]: Equal[A => A] = new Equal[A => A] {
    private val gen = implicitly[Gen[A]]
    private val rng = RNG.Simple(0)
    override def eq(l: A => A, r: A => A): Boolean = {
      val tRandoms = randomStream(gen)(rng).take(100)
      tRandoms.forall { v =>
        l(v) == r(v)
      }
    }
  }

  implicit def Func1Eq[A: Gen, B: Equal]: Equal[A => B] = new Equal[A => B] {
    private val gen = implicitly[Gen[A]]
    private val rng = RNG.Simple(0)
    private val eq = implicitly[Equal[B]]
    override def eq(l: A => B, r: A => B): Boolean = {
      val tRandoms = randomStream(gen)(rng).take(100)
      tRandoms.forall { v =>
        eq.eq(l(v), r(v))
      }
    }
  }

  implicit def GenEq[A]: Equal[Gen[A]] = new Equal[Gen[A]] {
    override def eq(l: Gen[A], r: Gen[A]): Boolean = {
      val tSimple = RNG.Simple(0)

      val tList1 = RNG.sequence(List.fill(100)(l.sample)).run(tSimple)._1
      val tList2 = RNG.sequence(List.fill(100)(r.sample)).run(tSimple)._1

      tList1 == tList2
    }
  }
}
