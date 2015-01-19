package io.pjan.scala.functional.fpinscala.errorhandling

sealed trait Either[+E, +A] {
  // Exercise 4.6: Implement `map`, `flatMap`, `orElse` and `map2`
  def map[B](f: A => B): Either[E, B] = this match {
    case l @ Left(_) => l
    case Right(a)    => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l @ Left(_) => l
    case Right(a)    => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r @ Right(_) => r
    case Left(_)      => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def left[E, A](value: E): Either[E, A] = Left(value)
  def right[E, A](value: A): Either[E, A] = Right(value)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // Exercise 4.7: Implement `sequence` and `traverse` for Either
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil    => Right(Nil)
    case h :: t => f(h).flatMap{ hh => traverse(t)(f).map{ hh :: _ } }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h.flatMap{ hh => sequence(t).map{ hh :: _ } }
  }

}
