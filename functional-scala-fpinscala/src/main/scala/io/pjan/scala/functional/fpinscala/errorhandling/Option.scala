package io.pjan.scala.functional.fpinscala.errorhandling

sealed trait Option[+A] {
  // Exercise 4.1: Implement `map`, `flatMap`, `getOrElse`, `orElse` and `filter`
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]) = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](b: => B): B = this match {
    case None    => b
    case Some(a) => a
  }

  def orElse[B >: A](b: => Option[B]): Option[B] = this match {
    case None    => b
    case Some(_) => this
  }

  def filter(pred: A => Boolean): Option[A] = this match {
    case Some(a) if pred(a) => this
    case _                  => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def apply[A](v: A): Option[A] = Some(v)
  def empty[A]: Option[A] = None

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2: Implement `variance` in terms of `flatMap`
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap{m => mean(xs.map{x => math.pow(x - m, 2)})}

  def lift[A, B](f: A => B): Option[A] => Option[B] = a => a map f

  // Exercise 4.3: Implement a generic function `map2` that combines 2 Option values using a binary function
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _)          => None
    case (_, None)          => None
    case (Some(a), Some(b)) => Some(f(a,b))
  }

  // Exercise 4.4: Implement `sequence`, a function that combines a list of Options into an Option containing a list of all the Some values.
  // If the original list contains None even once, the result should be None
  def sequence[A](os: List[Option[A]]): Option[List[A]] = os match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap{ hh => sequence(t) map (hh :: _) }
  }

  // Exercise 4.5: Implement `traverse`, with the following signature
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap{ hh => traverse(t)(f) map (hh :: _)}
  }

}
