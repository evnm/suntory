package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.mutable.Buffer

@RunWith(classOf[JUnitRunner])
class BufferingTransformerTest extends FunSuite {
  test("BufferingTransformer.apply: should buffer input until `consume` function is defined on input") {
    val n = 5
    val transformer = BufferingTransformer[Int, Int] {
      case input if input.size >= n =>
        (input.take(n).sum, input.drop(n))
    }
    for (i <- 1 to n-1) {
      assert(transformer(i) === None)
    }
    assert(transformer(n) === Some((1 to n).sum))
  }

  test("BufferingTransformer.andThen: should compose this transformer with another") {
    val t1 = BufferingTransformer[Int, Long] {
      case input if input.sum >= 10 => (input.sum.toLong, Seq.empty)
    }

    val t2 = BufferingTransformer[Long, String] {
      case input => (input(0).toString, input.tail)
    }

    val composed = t1 andThen t2

    assert(composed(9) === None)
    assert(composed(5) === Some("14"))
  }
}
