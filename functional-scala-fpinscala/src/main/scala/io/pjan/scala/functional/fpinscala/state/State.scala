package io.pjan.scala.functional.fpinscala.state


case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10: Generalize the functions `unit`, `map`, `map2`, `flatMap`, and `sequence`.
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })
  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = this.run(s)
    val (b, s2) = that.run(s1)
    (f(a, b), s2)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = this.run(s)
    f(a).run(s1)
  })

  def mapViaFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def mapViaFor[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2ViaFlatMap[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => that.map(b => f(a, b)))

  def map2ViaForComprehension[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- that
  } yield f(a, b)
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def init[S]: State[S, S] = State(s => (s, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List[A]()))((s, acc) => s.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}


object RAND {
  type Rand[A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = State(rng => (a, rng))

  def int: Rand[Int] = State(rng => rng.nextInt)

  def ints(count: Int): Rand[List[Int]] = State(rng => {
    @annotation.tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (acc.reverse, rng)
      } else {
        val (i, rng2) = int.run(rng)
        loop(count - 1, rng2, i :: acc)
      }
    }
    loop(count, rng, List())
  })

  def nonNegativeInt: Rand[Int] = int.map{ i =>
    if (i < 0) -(i+1) else i
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap{ i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }

  def randInts(n: Int): Rand[List[Int]] = for {
    x <- nonNegativeLessThan(n)
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)
}


// Exercise 6.11: implement a finite state automaton that models a simple candy dispenser.
// The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
// It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it contains.
object CandyMachine {
  import State._

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class CoinCount(value: Int) extends AnyVal {
    def -(i: Int) = CoinCount(value - i)
    def +(i: Int) = CoinCount(value + i)
  }
  case class CandyCount(value: Int) extends AnyVal {
    def -(i: Int) = CandyCount(value - i)
    def +(i: Int) = CandyCount(value + i)
  }

  case class Machine(locked: Boolean, candies: CandyCount, coins: CoinCount)

  def machineTransitions(m: Machine, i: Input): Machine = (m, i) match {
    case (Machine(_, CandyCount(0), _), _)   => m
    case (Machine(true, _, _), Turn)         => m
    case (Machine(false, _, _), Coin)        => m
    case (Machine(true, candy, coin), Coin)  => Machine(locked = false, candy, coin + 1)
    case (Machine(false, candy, coin), Turn) => Machine(locked = true, candy - 1, coin)

  }

  def simulateMachine(inputs: List[Input]): State[Machine, (CandyCount, CoinCount)] = for {
    _ <- sequence(inputs.map(i => modify((m: Machine) => machineTransitions(m, i))))
    s <- get
  } yield (s.candies, s.coins)


  def main(args: Array[String]): Unit = {
    // 3 successful candy dispenses
    val inputs = List(Coin, Turn, Turn, Coin, Turn, Coin, Coin, Turn)
    // starting with a machine with 20 candies and 10 coins
    val machine = Machine(locked = true, CandyCount(20), CoinCount(10))
    // the simulation
    val simulation = simulateMachine(inputs)
    // running it with the machine
    val res = simulation.run(machine)
    // print the result
    println(res)
  }
}
