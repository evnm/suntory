package net.evanmeagher.suntory

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ProtocolTest extends FunSuite {
  val builder = (seq: Seq[Int]) => seq.mkString(",")
  case class TestProducerState(self: Seq[Int])
    extends ProducerState[Int, String](self, builder)

  test("ProducerState: should apply `builder` to produce a value") {
    assert(TestProducerState(Seq()).value === builder(Seq()))
    assert(TestProducerState(Seq(1)).value === builder(Seq(1)))
    assert(TestProducerState(Seq(4,5,6)).value === builder(Seq(4,5,6)))
  }

  test("API sandbox") {
    // A state machine for summing five integers, then multiplying by two integers.
    case class Summing(self: Seq[Int])
      extends ProducerState[Int, Int](self, _.sum)

    case class Multiplying(self: Seq[Int])
      extends ProducerState[Int, Int](self, _.foldLeft(1)(_*_))

    val stateTable: PartialFunction[(State[Int], Int), Transition[Int]] = {
      case (InitialState, i) => GoTo(Summing(Seq(i)))
      case (Summing(seq), i) if seq.size < 5 => GoTo(Summing(i +: seq))
      case (s@Summing(_), i) => GoTo(Multiplying(Seq(s.value, i)))
      case (Multiplying(seq), i) if seq.size <= 2 => GoTo(Multiplying(i +: seq))
      case (Multiplying(_), _) => Loop
    }

    val machine = new Automaton[Int, Int](stateTable)

    for (i <- 1 to 7)
      println(machine(i))
  }
}
