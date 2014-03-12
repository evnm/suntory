package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class DelimitingTransformerTest extends FunSuite {
  test("DelimitingTransformer.apply: should split input stream on delimiter") {
    val n = 5
    val transformer = new DelimitingTransformer[Int](0)

    assert(transformer(Seq(1,2,3)) === Nil)
    assert(transformer(Seq(0,4)) === Seq(1,2,3))
    assert(transformer(Seq(5,0)) === Seq(4,5))
  }

  test("LineReadingTransformer.apply: should split input stream on newline characters") {
    val transformer = new LineReadingByteTransformer

    // Note: Arrays aren't directly comparable, to we have to `toSeq` them.
    assert(transformer("hello\nthere".getBytes) === "hello".getBytes.toSeq)
    assert(transformer("\nfoo".getBytes) === "there".getBytes.toSeq)
  }

  test("LineReadingTransformer.andThen(CharEncodingByteTransformer): should split input stream into line strings") {
    val lineDecoder = (new LineReadingByteTransformer)
      .andThen(new CharEncodingByteTransformer("UTF-8"))

    assert(lineDecoder("hello\nthere".getBytes).mkString === "hello")
    assert(lineDecoder("\nfoo".getBytes).mkString === "there")
  }
}
