package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.mutable.Buffer

@RunWith(classOf[JUnitRunner])
class DelimitingTransformerTest extends FunSuite {
  test("DelimitingTransformer.apply: should split input stream on delimiter") {
    val n = 5
    val transformer = DelimitingTransformer[Int](0)

    assert(transformer(Seq(1,2,3)) === None)
    assert(transformer(Seq(0,4)) === Some(Seq(1,2,3)))
  }
}
