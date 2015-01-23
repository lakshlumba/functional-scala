package io.pjan.scala.functional.fpinscala.state


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = rng => rng.nextInt

  // Exercise 6.1: Implement `RNG.nextInt` to generate a random integer between 0 and Int.maxValue (inclusive).
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) - (i + 1) else i, r)
  }

  // Exercise 6.2: Implement `double`, a function to generate a Double between 0 and 1, not including 1.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, rng2) = intDouble(rng)
    (t.swap, rng2)
  }

  def doubleDoubleDouble(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4: Write a function to generate a list of random Integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (i, rng2) = int(rng)
      (i :: ints(count - 1)(rng2)._1, rng2)
    }
  }

  // or tail-recursive
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (acc.reverse, rng)
      } else {
        val (i, rng2) = int(rng)
        loop(count - 1, rng2, i :: acc)
      }
    }
    loop(count, rng, List())
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // Exercise 6.5: implement those functions above (that allow it) using `map`
  def randNonNegativeInt: Rand[Int] =
    map(int)(i => if (i < 0) -(i + 1) else i)

  def randDouble: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6: Write the implementation of `map2` based on the following signature
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // Exercise 6.7: Implement `Sequence`, for combining a list of transitions into a single transition
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  // Exercise 6.8: Implement `flatMap` and use it to implement nonNegativeLessThan
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def rollDie: Rand[Int] = nonNegativeLessThan(6)
}