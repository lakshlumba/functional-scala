package io.pjan.scala.functional.fpinscala.testing

import Prop._
import io.pjan.scala.functional.fpinscala.state.RNG
import io.pjan.scala.functional.fpinscala.laziness.Stream

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case object Proved extends Result {
  def isFalsified = false
}
case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 8.9: Implement && and || for chaining props logically
  def &&(p2: Prop): Prop = Prop { (max, n, rng) =>
    this.run(max, n, rng) match {
      case Passed => p2.run(max, n, rng)
      case x      => x
    }
  }

  def ||(p2: Prop): Prop = Prop { (max, n, rng) =>
    this.run(max, n, rng) match {
      case Failed(_, _) => p2.run(max, n, rng)
      case x            => x
    }
  }

}


object Prop {
  type MaxSize      = Int
  type TestCases    = Int
  type SuccessCount = Int
  type FailedCase   = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Failed(a.toString, i)
      } catch {
        case e: Exception => Failed(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
    props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
    prop.run(max,n,rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Failed("()", 0)
  }

}