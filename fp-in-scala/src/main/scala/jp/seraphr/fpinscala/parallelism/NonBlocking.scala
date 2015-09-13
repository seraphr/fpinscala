package jp.seraphr.fpinscala.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference

/**
 */
object Nonblocking extends ParApi {
  trait CallbackFuture[A] {
    private[Nonblocking] def apply(callback: A => Unit): Unit
  }

  override type Future[A] = CallbackFuture[A]

  override def get[A](f: Future[A]): A = {
    val tRef = new AtomicReference[A]()
    val tLatch = new CountDownLatch(1)
    f { a =>
      tRef.set(a)
      tLatch.countDown()
    }

    tLatch.await()
    tRef.get()
  }

  override def unit[A](a: A): Par[A] = s => {
    new Future[A] {
      override private[Nonblocking] def apply(callback: A => Unit): Unit = callback(a)
    }
  }

  private def eval(s: ExecutorService)(r: => Unit): Unit = s.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

  override def fork[A](a: => Par[A]): Par[A] = s => {
    new Future[A] {
      override private[Nonblocking] def apply(callback: A => Unit): Unit = {
        eval(s)(a(s)(callback))
      }
    }
  }

  override def delay[A](a: => Par[A]): Par[A] = s => {
    new Future[A] {
      override private[Nonblocking] def apply(callback: (A) => Unit): Unit = a(s)(callback)
    }
  }

  override def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = s => {
    new Future[C] {
      override private[Nonblocking] def apply(callback: (C) => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](s) {
          case Left(a) => br match {
            case Some(b) => eval(s)(callback(f(a, b)))
            case None    => ar = Some(a)
          }
          case Right(b) => ar match {
            case Some(a) => eval(s)(callback(f(a, b)))
            case None    => br = Some(b)
          }
        }

        pa(s)(a => combiner ! Left(a))
        pb(s)(b => combiner ! Right(b))
      }
    }
  }
}
