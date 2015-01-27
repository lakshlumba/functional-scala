package io.pjan.scala.functional.fpinscala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

trait Par {
  type Par[A] // the representation type
  type Res[A] // the result type of run

  def run[A](es: ExecutorService)(pa: Par[A]): Res[A]

  def unit[A](a: A): Par[A]

  def fork[A](pa: => Par[A]): Par[A]

  def delay[A](pa: => Par[A]): Par[A]

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C]

  def map[A, B](pa: Par[A])(f: A => B): Par[B]

  def sequence[A](as: List[Par[A]]): Par[List[A]]

  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]]

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]]

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))
}


