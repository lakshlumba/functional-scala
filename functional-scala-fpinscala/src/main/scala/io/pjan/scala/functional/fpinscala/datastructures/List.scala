package io.pjan.scala.functional.fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // Exercise 3.1: Result of the match expression: 3

  // Exercise 3.2: Implement the function `tail`
  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => throw new Exception("tail of empty list")
    case Cons(x, xs) => xs
  }

  // Exercise 3.3: Implement the function `setHead` for replacing the first element of a list with a different value
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => throw new Exception("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }
}