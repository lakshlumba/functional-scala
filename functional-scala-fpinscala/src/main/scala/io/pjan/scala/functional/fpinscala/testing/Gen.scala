package io.pjan.scala.functional.fpinscala.testing

import io.pjan.scala.functional.fpinscala.state.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(this.sample.map(f))

  def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(this.sample.map2(that.sample)(f))

  // Exercise 8.6: Implement `flatMap` and use it to create a more dynamic version of `listOfN` taking a Gen[Int] as parameter
  def flatMap[B](f: A => Gen[B]) =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(size)(sample)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => listOfN(n))

  // Exercise 8.10: Implement `unsized`, a helper functions for converting Gen to SGen
  def unSized: SGen[A] =
    SGen(_ => this)

  def **[B](that: Gen[B]): Gen[(A, B)] =
    this.map2(that)((_, _))
}

object Gen {
  // Exercise 8.5: Try implementing unit, boolean, and listOfN.
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def int: Gen[Int] =
    Gen(State(RNG.int))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def choose(start: Double, stopExclusive: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => start + d * (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def toOption[A](ga: Gen[A]): Gen[Option[A]] =
    Gen(ga.sample.map(a => Option(a)))

  def ascii(length: Int): Gen[String] =
    listOfN(length, choose(1, 127)).map(_.map(_.toChar).mkString)

  def toOptionWithMap[A](ga: Gen[A]): Gen[Option[A]] =
    ga.map(a => Option(a))

  // Exercise 8.7: Implement union, for combining two generators of the same type into one,
  // by pulling values from each generator with equal likelihood.
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8: Implement weighted, a version of union that accepts a weight for each Gen and generates
  // values from each Gen with probability proportional to its weight.
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    double.flatMap(d => if (d < threshold) g1._1 else g2._1)
  }
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  // Exercise 8.11: Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen.
  def map[B](f: A => B): SGen[B] =
    SGen(g.andThen(_.map(f)))

  def flatMapGen[B](f: A => Gen[B]): SGen[B] =
    SGen(g.andThen(_.flatMap(f)))

  def **[B](that: SGen[B]): SGen[(A, B)] =
    SGen(n => this(n) ** that(n))
}

object SGen {
  // Exercise 8.12: Implement a listOf combinator that doesn’t accept an explicit size.
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  // Exercise 8.13: Implement an alternative to listOf which always gives non-empty lists
  def nonEmptyListOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n.max(1)))

  def ascii: SGen[String] = SGen(n => Gen.ascii(n))
}