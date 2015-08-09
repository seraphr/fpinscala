package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.laziness.Streams
import jp.seraphr.fpinscala.state.{ State, RNG }

/**
 */
case class Gen[A](sample: State[RNG, A], exhaustive: Option[Stream[A]]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample), exhaustive.map {
      _.flatMap(a => f(a).exhaustive.getOrElse(Stream.empty))
    })
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    if (stopExclusive <= start)
      throw new RuntimeException(s"start(=${start}) should be < stopExclusive(=${stopExclusive})")
    val tRange = stopExclusive - start
    Gen(RNG.nonNegativeLessThan(tRange).map(_ + start), Some(Streams.from(start).take(stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(RNG.unit(a), Some(Stream(a)))

  def boolean: Gen[Boolean] = Gen(RNG.boolean, Some(Stream(true, false)))

  def double: Gen[Double] = Gen(RNG.double, None)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val tExhaustive =
      g.exhaustive.map { s0 =>
        Streams.constant(s0).take(n).foldRight(Stream(Stream.empty[A])) {
          case (s, acc) =>
            for {
              accS <- acc
              v <- s
            } yield v #:: accS
        }.map(_.toList)
      }

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
