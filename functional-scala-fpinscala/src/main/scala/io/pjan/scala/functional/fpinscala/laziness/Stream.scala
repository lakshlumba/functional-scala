package io.pjan.scala.functional.fpinscala.laziness

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Cons(h, t) => Option(h())
    case _          => None
  }

  // Exercise 5.1: write a function `toList` which conversts a `Stream` to a `List`
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _          => Nil
  }

  def loListTailRecursive: List[A] = {
    @annotation.tailrec
    def loop[A](s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _          => acc
    }
    loop(this, List.empty).reverse
  }

  // Exercise 5.2: write a function `take` for returning the first n elements of a Stream and
  // `drop` to return a stream that skips the first n elements
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // Exercise 5.3: implement `takeWhile` for returning all starting elements of a stream matching the given predicate
  def takeWhile(pred: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if pred(h()) => cons(h(), t().takeWhile(pred))
    case _                       => empty
  }

  def exists(pred: A => Boolean): Boolean = this match {
    case Cons(h, t) => pred(h()) || t().exists(pred)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists2(pred: A => Boolean): Boolean =
    this.foldRight(false)((h, t) => pred(h) || t)

  // Exercise 5.4: Implement `forAll`, which checks whether all elements in a stream match a given predicate.
  // The traversal should stop from the moment a nonmatching value is encountered
  def forAll(pred: A => Boolean): Boolean =
    this.foldRight(true)((h, t) => pred(h) && t)

  // Exercise 5.5: Implement `takeWhile` using `foldRight`
  def takeWhile2(pred: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t) => if (pred(h)) cons(h, t) else empty)

  // Exercise 5.6: Implement `headOption` using `foldRight`
  def headOption2: Option[A] =
    this.foldRight(Option.empty[A])((h, _) => Some(h))

  // Exercise 5.7: Implement `map`, `append`, `filter`, and `flatMap` using foldRight
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((h, t) => cons(h, t))

  def filter(pred: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (pred(h)) cons(h, t) else t)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(pred: A => Boolean): Option[A] =
    filter(pred).headOption

  // Exercise 5.13: use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith` and `zipAll`. zipAll should continue
  // the traversal as long as either stream has more elements—it uses Option to indicate whether each stream has been exhausted.
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }

  def take2(n: Int): Stream[A] =
    unfold((n, this)) {
      case (nn, Cons(h, t)) if n == 1 => Some(h(), (nn - 1, empty))
      case (nn, Cons(h, t)) if n > 1  => Some(h(), (nn - 1, t()))
      case _                          => None
    }

  def takeWhile3(pred: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if pred(h()) => Some((h(), t()))
      case _                       => None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Empty, _)                   => None
      case (_, Empty)                   => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    zipWith(that)((_, _))

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty)               => None
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  // Exercise 5.14: Implement `startsWith` using previously written functions
  def startsWith[B >: A](that: Stream[B]): Boolean =
    zipAll(that).takeWhile(_._2.nonEmpty).forAll{ case(h1, h2) => h1 == h2 }

  // Exercise 5.15: Implement `tails`
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(h, t) => Some((s, t()))
      case Empty          => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

  // Exercise 5.16: Generalize `tails` to the function `scanRight`, which is like a `foldRight` that returns a stream of the intermediate results.
  def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] =
    foldRight((z, Stream(z))){ (h, t) =>
      lazy val tt = t
      val bb = f(h, tt._1)
      (bb, cons(bb, tt._2))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8: Generalize ones to a function`constant` which returns an infinite stream of a given value
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Exercise 5.9: Write a function `from` that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // Exercise 5.10: Write a function `fibs` that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def calc(n1: Int, n2: Int): Stream[Int] = Stream.cons(n1, calc(n2, n1 + n2))
    calc(0, 1)
  }

  // Exercise 5.11: implement `unfold`, a stream-building function that takes an initial state, as well as a function for both
  // generating the next state as well as the next value in the generated stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None         => Stream.empty[A]
  }

  // Exercise 5.12: Write `fibs`, `from`, `constant`, and `ones` in terms of unfold
  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some(n, n+1))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(s => Some(a, a))

  val ones2 = unfold(1)(_ => Some(1, 1))

  def fibs2: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
}