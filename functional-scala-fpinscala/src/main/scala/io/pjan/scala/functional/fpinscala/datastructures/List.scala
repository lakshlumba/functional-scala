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

  // Exercise 3.4: Generalize tail by mplementing `drop`
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)         => Nil
    case (_, d) if d <= 0 => l
    case (Cons(h, t), d)  => drop(t, d-1)
  }

  // Exercise 3.5: Implement `dropWhile`, removing elements from the list as long as they match a predicate
  @annotation.tailrec
  def dropWhile[A](l: List[A], pred: A => Boolean): List[A] = l match {
    case Nil                   => Nil
    case Cons(h, t) if pred(h) => dropWhile(t, pred)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil        => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  // Exercise 3.6: Implement `init`, a function that contains all but the last element of a list
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => throw new Exception("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  // Exercise 3.7: Can you short-circuit a foldRight calculation?
  // NO -> Every foldRight that is executed, first needs to evaluate it's arguments (and thus the underlying foldRights). Lazy evaluation is needed to do this

  // Exercise 3.8: What do you get when you foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))?
  // You get the same list back. You can think of foldRight as an operation where you replace the `Nil` with `z`, and the `Cons` with the applied function

  // Exercise 3.9: Compute the length of a list using foldRight
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  // Exercise 3.10: Implement foldLeft
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11: write `sum`, `product` and `length` using `foldLeft`
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // Exercise 3.12: implement `reverse` which, given a list, returns the reverse
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List.empty[A])((b, a) => Cons(a, b))

  // Exercise 3.13:
  // a) write `foldLeft` in terms of `foldRight`
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // Exercise 3.14: implement `append` in terms of either foldLeft or foldRight
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  // Exercise 3.15: implement `concat`, a function that concatenates a list of lists into a single list
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, List.empty[A])(append)

  // Exercise 3.16: implement `add1`, a function that adds 1 to each element of a list of integers
  def add1(ints: List[Int]): List[Int] = foldRight(ints, List.empty[Int])((a, b) => Cons(a + 1, b))

  // Exercise 3.17: implement `doublesToStrings`, a function that converts a list of doubles into a list of Strings
  def doublesToStrings(ds: List[Double]): List[String] = foldRight(ds, List.empty[String])((a, b) => Cons(a.toString, b))

  // Exercise 3.18: write a function `map`, that generalizes modifying each element, while maintaining the structure of the list
  def map1[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map1(t)(f))
  }

  def map2[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List.empty[B])((a, b) => Cons(f(a), b))

  def map3[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, List.empty[B])((a, b) => Cons(f(a), b))

  def map4[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = collection.mutable.ListBuffer.empty[B]
    @annotation.tailrec
    def loop(l: List[A]): List[B] = l match {
      case Nil        => List(buf.toList: _*)
      case Cons(h, t) => buf += f(h); loop(t)
    }
    loop(l)
  }

  // Exercise 3.19: write a function called `filter`, which removes all the elements from a list unless they satisfy a predicate
  def filter[A](l: List[A])(pred: A => Boolean): List[A] = l match {
    case Nil                   => Nil
    case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    case Cons(_, t)            => filter(t)(pred)
  }

  // Exercise 3.20: write a function `flatMap` with the following signature
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map4(l)(f))

  // Exercise 3.21: implement `filter` using `flatMap`
  def filter[A](l: List[A])(pred: A => Boolean): List[A] =
    flatMap(l)(a => if (pred(a)) List(a) else Nil)

  // Exercise 3.22: implement a function that takes 2 lists of Ints, and produces a new list of Ints by adding the corresponding elements
  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  // Exercise 3.23: Generalize `addPairWise` and call it zipWith
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // Exercise 3.24: TODO

}