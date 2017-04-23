package jp.seraphr.fpinscala.monoid

import jp.seraphr.fpinscala.monads.Monad
import jp.seraphr.fpinscala.prop.{ Gen, Prop }
import jp.seraphr.fpinscala.state.State
import jp.seraphr.fpinscala.utils.Equal
import org.scalatest.{ Matchers, FreeSpec }

import scala.language.higherKinds

/**
 */
class MonadSpec extends FreeSpec with Matchers {
  import jp.seraphr.fpinscala.monads.MonadLaws._
  import Gen._
  import jp.seraphr.fpinscala.prop.Prop

  def testProp(aProp: Prop.Prop, aTestCases: Int = 100): Unit = {
    val tResult = Prop.run(aProp, testCases = aTestCases)
    tResult shouldBe 'right
    println(tResult)
  }

  def testMonadLaw[M[_]](m: Monad[M])(gen: Gen[M[Int]])(implicit eq: Equal[M[Int]]): Unit = {
    val tGenF = Gen.func1[Int, M[Int]](gen)
    val tGenG = Gen.func1[Int, M[Int]](gen)
    val tLaws = monadLaws[M, Int, Int, Int](m, Gen.choose(0, 1000), tGenF, tGenG)

    testProp(tLaws)
  }

  "EXERCISE 11.1" - {
    "genMonad" in {
      val tGenGen = Gen.tuple(Gen.choose(-1000, 0), Gen.choose(1000, 2000)).map {
        case (l, h) => Gen.choose(l, h)
      }
      testMonadLaw(Monad.genMonad)(tGenGen)
    }

    "parMonad  parの同値性チェック無理だよ…" ignore {
      //      testMonadLaw(Monad.parMonad)(Gen.unit(Gen.choose(-100, 100)))
    }

    "optionMonad" in {
      testMonadLaw(Monad.optionMonad)(Gen.option(Gen.choose(-1000, 500)))
    }

    "streamMonad" in {
      testMonadLaw(Monad.streamMonad)(Gen.listOfN(10, Gen.choose(1000, 2000)).map(_.toStream))
    }

    "listMonad" in {
      testMonadLaw(Monad.listMonad)(Gen.listOfN(10, Gen.choose(1000, 2000)))
    }
  }

  "EXERCISE 11.2" - {
    "stateMonad" in {
      // XXX runの引数固定だけど、とりあえずということで…
      type _M[X] = State[String, X]
      implicit def stateEq[A]: Equal[_M[A]] = new Equal[_M[A]] {
        override def eq(l: _M[A], r: _M[A]): Boolean = {
          l.run("hoge") == r.run("hoge")
        }
      }
      testMonadLaw[_M](Monad.stateMonad[String])(Gen.choose(-100, 100).map(State.unit))
    }
  }
}
