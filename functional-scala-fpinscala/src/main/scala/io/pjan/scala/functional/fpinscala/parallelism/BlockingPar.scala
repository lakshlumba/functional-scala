package io.pjan.scala.functional.fpinscala.parallelism

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object BlockingPar extends Par {
  type Par[A] = ExecutorService => Future[A]
  type Res[A] = Future[A]

  override def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  override def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  override def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  override def fork[A](pa: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A]{
      override def call(): A = pa(es).get
    })
  }

  override def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  // Exercise 7.5: Write sequence. No additional primitives are required. Do not call run.
  override def sequence[A](pss: List[Par[A]]): Par[List[A]] =
    pss.foldRight(unit(List[A]()))((pa, acc) => map2(pa, acc)(_ :: _))

  override def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fs = as.map(asyncF(f))
    sequence(fs)
  }

  override def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pas))(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(pa, map2(pb, pc)((b, c) => (b, c)))((a, bc) => f(a, bc._1, bc._2))
  }

  def delay[A](pa: => Par[A]): Par[A] =
    (es: ExecutorService) => pa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = (es: ExecutorService) => {
    if (run(es)(cond).get) t(es)
    else f(es)
  }

  // Exercise 7.11: Implement choiceN, and then choice in terms of choiceN
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = (es: ExecutorService) => {
    val index = run(es)(n).get
    run(es)(choices(index))
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  // Exercise 7.12: implement choiceMap, further generalising choiceN
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = (es: ExecutorService) => {
    val k = run(es)(key).get
    run(es)(choices(k))
  }

  // Exercise 7.13: finalize the generalisation, by implementing chooser, and use it to both implement choice & choiceN
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = (es: ExecutorService) => {
    val k = run(es)(pa).get
    run(es)(choices(k))
  }


  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = (es: ExecutorService) => {
    val a = run(es)(pa).get
    run(es)(f(a))
  }

  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(b => if (b) t else f)

  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(n => choices(n))

  // Exercise 7.14: Implement join. Can you implement flatMap using join? Can you implement join using flatMap
  def join[A](a: Par[Par[A]]): Par[A] = (es: ExecutorService) => {
    run(es)(run(es)(a).get())
  }

  def joinViaFlatmap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

}