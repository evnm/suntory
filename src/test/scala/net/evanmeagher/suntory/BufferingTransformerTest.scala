package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar
import org.junit.runner.RunWith
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class BufferingTransformerTest extends FunSuite {
  test("BufferingTransformer.apply: should buffer input until `consume` function is defined on input") {
    val n = 5
    val transformer = new BufferingTransformer[Int, Int]({
      // Sum the first `n` numbers.
      case input if input.size >= n =>
        (Seq(input.take(n).sum), input.drop(n))
    })
    for (i <- 1 to n-1) {
      assert(transformer(i) === Nil)
    }
    assert(transformer(n) === Seq((1 to n).sum))
  }

  test("BufferingTransformer.andThen: should compose this transformer with another") {
    val t1 = new BufferingTransformer[Int, Long]({
      case input if input.sum >= 10 => (Seq(input.sum.toLong), Seq.empty)
    })

    val t2 = new BufferingTransformer[Long, String]({
      case input => (Seq(input(0).toString), input.tail)
    })

    val composed = t1 andThen t2

    assert(composed(9) === Nil)
    assert(composed(5) === Seq("14"))
  }

  test("BufferingTransformer.reset: should clear underlying buffer") {
    // A transformer that buffers all input.
    val buf = new ArrayBuffer[Int]
    assert(buf.size == 0, "initial buffer size was non-zero")
    val transformer = new BufferingTransformer[Int, Int](
      { case input => (Seq.empty, input) },
      buf
    )

    assert(transformer(14) === Nil, "transformer output was non-nil")
    assert(buf.size == 1)
    transformer.reset()
    assert(buf.size == 0, "buffer size after `BufferingTransformer.reset()` was non-zero")
  }

  test("CharEncodingByteTransformer.apply: should convert byte-arrays to strings") {
    val encoder = new CharEncodingByteTransformer("UTF-8")

    assert(encoder("hello".getBytes).mkString === "hello")
    assert(encoder("there".getBytes).mkString === "there")
  }
}
