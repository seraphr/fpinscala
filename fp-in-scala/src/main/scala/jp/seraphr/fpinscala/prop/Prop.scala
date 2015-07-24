package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.laziness.Streams
import jp.seraphr.fpinscala.state.RNG

/**
 */
object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop { (tCases, tRng) =>
      val tFirst = this.run(tCases, tRng)
      if (tFirst.isFalsified)
        tFirst
      else
        p.run(tCases, tRng)
    }

    def ||(p: Prop): Prop = Prop { (tCases, tRng) =>
      val tFirst = this.run(tCases, tRng)
      if (!tFirst.isFalsified)
        tFirst
      else
        p.run(tCases, tRng)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Streams.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
