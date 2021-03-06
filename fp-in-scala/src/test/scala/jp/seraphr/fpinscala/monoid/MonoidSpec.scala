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

  def testProp(aProp: Prop.Prop, aTestCases: Int = 100): Unit = {
    val tResult = jp.seraphr.fpinscala.prop.Prop.run(aProp, testCases = aTestCases)
    tResult shouldBe 'right
    println(tResult)
  }

  "EXERCISE 10.1" - {

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

  "EXERCISE 10.11" - {
    "WordCount.count" - {
      "simple string" in {
        val tStringGen = Gen.unit("  aa  bb  c  ")
        val tProp = Prop.forAll(tStringGen)(s => WordCount.count(s) == 3)

        testProp(tProp)
      }

      "only space" in {
        val tStringGen = Gen.choose(0, 10).map(n => new String(Array.fill(n)(' ')))
        val tProp = Prop.forAll(tStringGen)(s => WordCount.count(s) == 0)
        testProp(tProp)
      }

      "random string" in {
        val tStringGen = Gen.choose(1, 100).flatMap(Gen.alphaString(_))
        val tProp = Prop.forAll(tStringGen)(s => WordCount.count(s) == 1)
        testProp(tProp)
      }

      "random string with space" in {
        val tStringGen = Gen.choose(100, 1000).flatMap(Gen.alphaSpaceString(_))
        val tProp = Prop.forAll(tStringGen) { s =>
          val tExpected = s.split(" +") match {
            case Array()           => 0
            case a @ Array("", _*) => a.length - 1
            case a                 => a.length
          }

          val tActual = WordCount.count(s)
          tActual == tExpected
        }

        testProp(tProp)
      }
    }
  }

  "EXERCISE 10.16" - {
    def testMonoidLaw[A, B](a: Monoid[A], b: Monoid[B], ga: Gen[A], gb: Gen[B]): Unit = {
      val tProduct = Monoid.productMonoid(a, b)
      val tGen = Gen.tuple(ga, gb)
      testProp(monoidLaws(tProduct, tGen))
    }

    "Int product Int" in {
      testMonoidLaw(Monoid.intAddition, Monoid.intMultiplication, Gen.choose(0, 100), Gen.choose(100, 200))
    }

    "Boolean product Boolean" in {
      testMonoidLaw(Monoid.booleanAnd, Monoid.booleanOr, Gen.boolean, Gen.boolean)
    }
  }

  "EXERCISE 10.17" in {
    val tMonoid = Monoid.functionMonoid[String, Int](Monoid.intAddition)
    val tGen = Gen.func1[String, Int](Gen.choose(-100, 100))
    implicit val tStrGen = Gen.alphaString(10)
    testProp(monoidLaws(tMonoid, tGen))
  }

  "EXERCIZE 10.18" in {
    val tVectorGen = Gen.listOf(Gen.choose(-100, 100)).map(_.toVector)
    val tProp = Prop.forAll(tVectorGen) { tVec =>
      val tBagResult = Monoid.bag(tVec)
      val tExpected = tVec.groupBy(identity).mapValues(_.size)

      tBagResult == tExpected
    }

    testProp(tProp)
  }
}