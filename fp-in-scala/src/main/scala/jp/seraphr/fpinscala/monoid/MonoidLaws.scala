package jp.seraphr.fpinscala.monoid

import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.state.RNG

/**
 */
object MonoidLaws {

  import Gen._
  import jp.seraphr.fpinscala.prop.Prop._

  trait Equals[A] {
    def eq(l: A, r: A): Boolean
  }

  trait WeakEqualsInstance {
    implicit def DefaultEq[A]: Equals[A] = new Equals[A] {
      override def eq(l: A, r: A): Boolean = l == r
    }
  }

  object Equals extends WeakEqualsInstance {
    implicit def EndoEq[A: Gen]: Equals[A => A] = new Equals[A => A] {
      private val gen = implicitly[Gen[A]]
      private val rng = RNG.Simple(0)
      override def eq(l: A => A, r: A => A): Boolean = {
        val tRandoms = randomStream(gen)(rng).take(100)
        tRandoms.forall { v =>
          l(v) == r(v)
        }
      }
    }
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A])(implicit evEq: Equals[A]): Prop = {
    implicit class MonoidOpt(l: A) {
      def op(r: A) = m.op(l, r)
      def ===(r: A) = evEq.eq(l, r)
    }

    val tAssociativeLaw = forAll(listOfN(3, gen)) {
      case List(a, b, c) =>
        ((a op b) op c) === (a op (b op c))
    }

    val tIdentityElementLaw1 = forAll(gen) { a =>
      (m.zero op a) === a
    }

    val tIdentityElementLaw2 = forAll(gen) { a =>
      (a op m.zero) === a
    }

    tAssociativeLaw && tIdentityElementLaw1 && tIdentityElementLaw2
  }
}
