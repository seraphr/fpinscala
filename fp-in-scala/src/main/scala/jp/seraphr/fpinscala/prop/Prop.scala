package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.laziness.Streams
import jp.seraphr.fpinscala.state.RNG

/**
 */
object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop { (tMaxSize, tCases, tRng) =>
      val tFirst = this.run(tMaxSize, tCases, tRng)
      if (tFirst.isFalsified)
        tFirst
      else
        p.run(tMaxSize, tCases, tRng)
    }

    def ||(p: Prop): Prop = Prop { (tMaxSize, tCases, tRng) =>
      val tFirst = this.run(tMaxSize, tCases, tRng)
      if (!tFirst.isFalsified)
        tFirst
      else
        (tFirst, p.run(tMaxSize, tCases, tRng)) match {
          case (Proved, Proved) => Proved
          case (_, r)           => r
        }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Streams.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (tMaxSize, n, rng) =>
      val tMaxExhaustive = n / 2
      @annotation.tailrec
      def runInner(i: Int, end: Int, valueStream: Stream[Option[A]], onEnd: Result): (Result, Int) = {
        if (i >= end) if (valueStream.isEmpty) (onEnd, i) else (Passed, i)
        else valueStream match {
          case Some(a) #:: as => if (f(a)) runInner(i + 1, end, as, onEnd) else (Falsified(a.toString, i), i)
          case None #:: as    => (Passed, i)
          case _              => (onEnd, i)
        }
      }

      def runExhaustive: Either[Int, Result] = {
        runInner(0, tMaxExhaustive, gen.exhaustive, Proved) match {
          case (Passed, i) => Left(n - i)
          case (r, i)      => Right(r)
        }
      }

      // Left(n) => 追加でn回テストを行う Right(r) => rを結果とする
      runExhaustive match {
        case Right(r) => r
        case Left(i)  => runInner(n - i, n, randomStream(gen)(rng).take(i).map(Some(_)), Passed)._1
      }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)

      // サイズ付きテストは、より大きいmaxに対する証明が行われないため、ProvedをPassedに変更
      prop.run(max, n, rng) match {
        case Proved => Passed
        case r      => r
      }
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Either[String, String] =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        Left(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        Right(s"+ OK, passed $testCases tests.")
      case Proved =>
        Right(s"+ OK, proved.")
    }
}
