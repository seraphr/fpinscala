package jp.seraphr.fpinscala.parallelism

import java.util.concurrent.{ Future, ExecutorService }
import language.higherKinds

/**
 */
trait ParApi { self =>
  type Par[A] = ExecutorService => Future[A]
  type Future[A]

  implicit class ParOpts[A](pa: Par[A]) {
    def run(s: ExecutorService): A = self.get(pa(s))
    def map[B](f: A => B) = self.map(pa)(f)
    def flatMap[B](f: A => Par[B]) = self.flatMap(pa)(f)
    def flatMap[B](a: Par[A])(f: A => Par[B]): Par[B] = self.flatMap[A, B](pa)(f)
  }

  def run[A](s: ExecutorService)(a: Par[A]): A = get(runFuture(s)(a))
  def runFuture[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def get[A](f: Future[A]): A
  def unit[A](a: A): Par[A]
  def fork[A](a: => Par[A]): Par[A]
  def delay[A](a: => Par[A]): Par[A]
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = s => {
    val ar = a.run(s)
    f(ar)(s)
  }
  def join[A](ppa: Par[Par[A]]): Par[A] = s => {
    ppa.run(s)(s)
  }

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(a.map(f))
  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = ppa.flatMap(a => a)

  def choiseMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    key.flatMap(choices)
  }
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    n.flatMap(choices)
  }
  def choice[A](b: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n = b.map(if (_) 0 else 1)
    choiceN(n)(List(t, f))
  }
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](p: Par[A])(f: A => B): Par[B] = map2(p, unit(()))((a, _) => f(a))
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }
  def parFilter[A](ps: List[A])(filter: A => Boolean): Par[List[A]] = fork(unit(ps.filter(filter)))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case x :: xs =>
      val tZero = map(x)(List(_))
      xs.foldLeft(tZero)((acc, p) => map2(p, acc)(_ :: _))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
}
