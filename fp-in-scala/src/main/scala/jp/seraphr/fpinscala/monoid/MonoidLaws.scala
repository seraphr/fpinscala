package jp.seraphr.fpinscala.monoid

import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.state.RNG
import jp.seraphr.fpinscala.utils.Equal

/**
 */
object MonoidLaws {

  import Gen._
  import jp.seraphr.fpinscala.prop.Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A])(implicit evEq: Equal[A]): Prop = {
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
