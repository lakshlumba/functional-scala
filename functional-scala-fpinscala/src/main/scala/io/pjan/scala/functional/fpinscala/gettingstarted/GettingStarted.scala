package io.pjan.scala.functional.fpinscala.gettingstarted

object GettingStarted {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n-1, acc * n)

    loop(n, 1)
  }

  // Exercise 1: write a function computing the nth fibonacci number.
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n-1, cur, cur+prev)

    loop(n, 0, 1)
  }

  // Exercise 2: implement a polymorphic `isSorted` function for arrays
  def isSorted[A](as: Array[A])(gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (gt(as(n), as(n-1))) false
      else loop(n + 1)

    loop(1)
  }

  // Exercise 3: implement `curry`
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  // Exercise 4: implement `uncurry`
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 5: implement `compose`
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
