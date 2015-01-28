package io.pjan.scala.functional.fpinscala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

object NonBlockingPar extends Par {

  sealed trait Future[+A] {
    private[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]
  type Res[A] = A

  override def run[A](es: ExecutorService)(pa: Par[A]): A =  {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    pa(es){ a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get
  }

  override def unit[A](a: A): Par[A] = (es: ExecutorService) => {
    new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
  }

  override def fork[A](pa: => Par[A]): Par[A] = (es: ExecutorService) => {
    new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es){ pa(es)(cb) }
    }
  }

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r})

  override def delay[A](pa: => Par[A]): Par[A] = (es: ExecutorService) => {
    pa(es)
  }

  override def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) => br match {
            case None    => ar = Some(a)
            case Some(b) => eval(es){ cb(f(a,b)) }
          }
          case Right(b) => ar match {
            case None    => br = Some(b)
            case Some(a) => eval(es){ cb(f(a,b)) }
          }
        }

        pa(es){ a => combiner ! Left(a) }
        pb(es){ b => combiner ! Right(b) }
      }
    }
  }

  override def map[A, B](pa: Par[A])(f: A => B): Par[B] = (es: ExecutorService) => {
    new Future[B]{
      def apply(cb: B => Unit): Unit = pa(es){ a => eval(es){ cb(f(a)) } }
    }
  }

  override def sequence[A](as: List[Par[A]]): Par[List[A]] = ???

  override def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = ???

  override def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???
}
