package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.mutable.Buffer

@RunWith(classOf[JUnitRunner])
class BufferingTransformerTest extends FunSuite {
  test("BufferingTransformer.apply: Buffers input until `consume` function is defined on input") {
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
}
