package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.datastructures.{ Lists, Options }
import jp.seraphr.fpinscala.laziness.Streams
import jp.seraphr.fpinscala.state.{ State, RNG }

/**
 *
 * @param sample
 * @param exhaustive 対象ドメインの全領域を数え上げるストリーム。 要素中に一つでもNoneが含まれる場合、数え上げが不可能なことを表す
 * @tparam A
 */
case class Gen[A](sample: State[RNG, A], exhaustive: Stream[Option[A]]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val tExhaustive = exhaustive.flatMap { so =>
      Options.sequenceStream(so.map(a => f(a).exhaustive)).map(_.flatten)
    }

    Gen(sample.flatMap(a => f(a).sample), tExhaustive)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  private def wrap[A](a: A): Option[A] = Some(a)

  private val genericNoExhaustive = Stream(None)

  private def noExhaustive[A]: Stream[Option[A]] = genericNoExhaustive

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    if (stopExclusive <= start)
      throw new RuntimeException(s"start(=${start}) should be < stopExclusive(=${stopExclusive})")
    val tRange = stopExclusive - start
    Gen(RNG.nonNegativeLessThan(tRange).map(_ + start), Streams.from(start).take(stopExclusive - start).map(wrap))
  }

  def unit[A](a: => A): Gen[A] = Gen(RNG.unit(a), Stream(Some(a)))

  def boolean: Gen[Boolean] = Gen(RNG.boolean, Stream(true, false).map(wrap))

  def double: Gen[Double] = Gen(RNG.double, noExhaustive)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val tExhaustive = Streams.constant(g.exhaustive).take(n).foldRight(Stream(Stream.empty[Option[A]])) {
      case (s, acc) =>
        for {
          accS <- acc
          v <- s
        } yield v #:: accS
    }.map(s => Lists.sequenceOpt(s.toList))

    Gen(RNG.sequence(List.fill(n)(g.sample)), tExhaustive)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n max 1, g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val tRatio = g1._2 / (g1._2 + g2._2)

    double.flatMap(d => if (d < tRatio) g1._1 else g2._1)
  }
}
