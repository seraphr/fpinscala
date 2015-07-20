package jp.seraphr.fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = State[RNG, A]
  val int: Rand[Int] = State(r => r.nextInt)
  def unit[A](a: A): Rand[A] = State.unit(a)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State{ r =>
    val (a, r1) = ra.run(r)
    val (b, r2) = rb.run(r1)

    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case r :: rs => map2(r, sequence(rs))(_ :: _)
    case _ => unit(List[A]())
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int)).run(rng)

  def nonNegativeInt: Rand[Int] = State { rng =>
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}