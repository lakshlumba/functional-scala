package io.pjan.scala.functional.fpinscala.testing

import io.pjan.scala.functional.fpinscala.state.RNG

object Examples {

  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(1241)

    val reverseStringProp = Prop.forAll(SGen.ascii)(s => s == s.reverse.reverse)

    val maxProp = Prop.forAll(SGen.nonEmptyListOf(Gen.int)) { l =>
      val max = l.max
      !l.exists(_ > max)
    }

    val sortedProp = Prop.forAll(SGen.listOf(Gen.int)) { l =>
      val sorted = l.sorted
      l.size <= 1 || !sorted.zip(sorted.tail).exists{ case(a,b) => a > b}
    }


    def run(prop: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit = {
      println(s"Tests running with rng: ${rng.toString}")
      prop.run(maxSize, testCases, rng) match {
        case Passed       => println(s"+ OK, passed $testCases tests.")
        case Proved       => println(s"+ OK, proved property")
        case Failed(f, n) => println(s"! Failed, after $n passed tests:\n $f")
      }
    }

    run(reverseStringProp, testCases = 200)
    run(maxProp, rng = rng)
    run(sortedProp)
  }
}
