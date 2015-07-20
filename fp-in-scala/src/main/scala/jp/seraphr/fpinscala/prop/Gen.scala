package jp.seraphr.fpinscala.prop

import jp.seraphr.fpinscala.state.{State, RNG}

/**
  */
case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    if(stopExclusive <= start)
      throw new RuntimeException(s"start(=${start}) should be < stopExclusive(=${stopExclusive})")
    val tRange = stopExclusive - start
    Gen(RNG.nonNegativeLessThan(tRange).map(_ + start))
  }
}
