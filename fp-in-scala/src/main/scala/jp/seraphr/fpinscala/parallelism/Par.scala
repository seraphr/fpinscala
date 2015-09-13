package jp.seraphr.fpinscala.parallelism

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent._

/**
 */
object Par extends ParApi {
  override type Future[A] = java.util.concurrent.Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  override def get[A](f: Future[A]): A = f.get()
  override def unit[A](a: A): Par[A] = s => UnitFuture(a)
  override def fork[A](a: => Par[A]): Par[A] = s => s.submit(new Callable[A] {
    override def call(): A = a(s).get()
  })
  override def delay[A](a: => Par[A]): Par[A] = s => a(s)

  override def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = s => new Future[C] {
    private val l = a(s)
    private val r = b(s)
    private val mIsDone = new AtomicBoolean(false)
    private val mIsCanceled = new AtomicBoolean(false)

    override def isCancelled: Boolean = mIsCanceled.get()
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      mIsCanceled.compareAndSet(false, l.cancel(mayInterruptIfRunning) | r.cancel(mayInterruptIfRunning))
      mIsCanceled.get()
    }

    override def isDone: Boolean = mIsDone.get()

    override def get(): C = get(Long.MaxValue, TimeUnit.MILLISECONDS)
    override def get(timeout: Long, unit: TimeUnit): C = {
      if (mIsCanceled.get())
        throw new CancellationException

      val tStartTime = System.currentTimeMillis()
      val a = l.get(timeout, unit)
      val tEndOfA = System.currentTimeMillis()
      val tRemain = unit.toMillis(timeout) - (tEndOfA - tStartTime)
      val b = r.get(tRemain, TimeUnit.MILLISECONDS)
      f(a, b)
    }
  }
  override def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = s => {
    val i = n(s).get
    choices(i)(s)
  }
}
