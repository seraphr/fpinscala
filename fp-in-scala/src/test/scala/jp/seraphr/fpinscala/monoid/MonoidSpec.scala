package jp.seraphr.fpinscala.monoid

import java.util.concurrent.Executors

import jp.seraphr.fpinscala.monoid.WordCount.{ Part, Stub }
import jp.seraphr.fpinscala.parallelism.Nonblocking
import jp.seraphr.fpinscala.prop.Gen
import jp.seraphr.fpinscala.prop.Prop
import jp.seraphr.fpinscala.state.RNG
import org.scalatest.{ FreeSpec, Matchers, FunSuite }
import language.higherKinds

/**
 */
class MonoidSpec extends FreeSpec with Matchers {

  import MonoidLaws._

  "EXERCISE 10.1" - {
    def testProp(aProp: Prop.Prop, aTestCases: Int = 100): Unit = {
      val tResult = jp.seraphr.fpinscala.prop.Prop.run(aProp, testCases = aTestCases)
      tResult shouldBe 'right
    }

    "intAddition" in {

      implicit val tGenInt = Gen.choose(-100, 100)
      testProp(monoidLaws(Monoid.intAddition, tGenInt))
      testProp(monoidLaws(Monoid.intAddition, tGenInt))
    }

    "booleanOr" in {
      implicit val tGenBool = Gen.boolean
      testProp(monoidLaws(Monoid.booleanOr, tGenBool))
    }

    "booleanAnd" in {
      implicit val tGenBool = Gen.boolean
      testProp(monoidLaws(Monoid.booleanAnd, tGenBool))
    }

    "EXERCISE 10.2" - {
      "option" in {
        implicit val tGenBool = Gen.boolean
        implicit val tGenInt = Gen.choose(-100, 100)

        testProp(monoidLaws(Monoid.optionMonoid[Int], Gen.option(tGenInt)))
        testProp(monoidLaws(Monoid.optionMonoid[Boolean], Gen.option(tGenBool)))
      }
    }

    "EXERCISE 10.3" - {
      "endoMonoid" in {
        implicit val tGenBool = Gen.boolean
        implicit val tGenInt = Gen.choose(-100, 100)
        val tGenIntEndo = Gen.func1[Int, Int](tGenInt)
        testProp(monoidLaws(Monoid.endoMonoid[Int], tGenIntEndo))

        val tGenBoolEndo = Gen.func1[Boolean, Boolean](tGenBool)
        testProp(monoidLaws(Monoid.endoMonoid[Boolean], tGenBoolEndo))
      }
    }

    "EXERCISE 10.5" - {
      "foldMap" in {
        val tStringGen = Gen.choose(0, 100).flatMap(Gen.alphaString(_))
        val tStringListGen = Gen.listOf(tStringGen)

        val tProp = Prop.forAll(tStringListGen) { tStrings =>
          val tActual = Monoid.foldMap(tStrings, Monoid.intAddition)(_.size)
          val tExpected = tStrings.map(_.length).sum

          tActual == tExpected
        }

        testProp(tProp)
      }
    }

    def testFoldMap[A, B, S[_]](f: List[String] => S[String])(foldMap: (S[String], Monoid[Int]) => (String => Int) => Int): Unit = {
      val tStringGen = Gen.choose(0, 100).flatMap(Gen.alphaString(_))
      val tStringListGen = Gen.listOf(tStringGen)

      val tProp = Prop.forAll(tStringListGen) { tStrings =>
        val tActual = foldMap(f(tStrings), Monoid.intAddition)(_.size)
        val tExpected = Monoid.foldMap(tStrings.toList, Monoid.intAddition)(_.size)

        tActual == tExpected
      }

      testProp(tProp)
    }
    def testIndexSeqFoldMap[A, B](foldMap: (IndexedSeq[String], Monoid[Int]) => (String => Int) => Int): Unit = testFoldMap(_.toIndexedSeq)(foldMap)
    def testListFoldMap[A, B](foldMap: (List[String], Monoid[Int]) => (String => Int) => Int): Unit = testFoldMap(_.toList)(foldMap)

    "EXERCISE 10.6" - {
      "foldMap" - {

        "foldMapViaFoldLeft" in {
          testListFoldMap(Monoid.foldMapViaFoldLeft)
        }

        "foldMapViaFoldRight" in {
          testListFoldMap(Monoid.foldMapViaFoldRight)
        }
      }

      "foldLeftViaFoldMap" - {
        val tProp = Prop.forAll(Gen.listOf(Gen.choose(-100, 100))) { tList =>
          val tActual = Monoid.foldLeftViaFoldMap(tList)(List.empty[Int])((l, e) => e :: l)
          val tExpected = tList.reverse

          tActual == tExpected
        }

        testProp(tProp)
      }

      "foldRightViaFoldMap" - {
        val tProp = Prop.forAll(Gen.listOf(Gen.choose(-100, 100))) { tList =>
          val tActual = Monoid.foldRightViaFoldMap(tList)(List.empty[Int])(_ :: _)
          val tExpected = tList

          tActual == tExpected
        }
        testProp(tProp)
      }
    }

    "EXERCISE 10.7" - {

      "foldMapV" in {
        testIndexSeqFoldMap(Monoid.foldMapV)
      }
    }

    "EXERCISE 10.8" - {
      "parFoldMap" in {
        val tExecutor = Executors.newSingleThreadExecutor()
        def getValue[A](p: Nonblocking.Par[A]): A = Nonblocking.get(p(tExecutor))
        def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = getValue(Monoid.parFoldMap(v, m)(f))

        try {
          testIndexSeqFoldMap(foldMap)
        } finally {
          tExecutor.shutdown()
        }
      }
    }

    "EXERCISE 10.9" - {
      "isSorted" in {
        val tProp1 = Prop.forAll(Gen.listOf(Gen.choose(-1000, 1000)).map(_.toVector)) { tVector =>
          val tSortedVector = tVector.sorted

          Monoid.isSorted(tSortedVector) == true
        }

        val tProp2 = Prop.forAll(Gen.listOf(Gen.choose(-1000, 1000)).map(_.toVector)) { tVector =>
          val tSortedVector = tVector.sorted

          Monoid.isSorted(tVector) == (tSortedVector == tVector)
        }
        testProp(tProp1 && tProp2, 500)
      }
    }

    "EXERCISE 10.10" - {
      "wcMonoid" in {
        val tIntGen = Gen.choose(0, 100)
        val tStringGen = Gen.choose(0, 100).flatMap(Gen.alphaString(_))
        val tStubGen = tStringGen.map(WordCount.stub)
        val tPartGen = for {
          l <- tStringGen
          wc <- tIntGen
          r <- tStringGen
        } yield WordCount.part(l, wc, r)

        val tWCGen = Gen.union(tStubGen, tPartGen)
        val tMonoidProp = monoidLaws(WordCount.wcMonoid, tWCGen)

        testProp(tMonoidProp, 100)
      }

    }
  }
}