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
        p.run(tMaxSize, tCases, tRng)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Streams.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (tMaxSize, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
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
      prop.run(max, n, rng)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}
