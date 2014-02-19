package net.evanmeagher.suntory

import java.nio.ByteBuffer
import scala.collection.SeqProxy

object Protocol {
  /**
   * A base trait for state machine transitions.
   */
  sealed trait Edge

  /**
   * A self-loop. No consumable input, so we remain at the current state.
   */
  case object Loop extends Edge

  /**
   * Walk an edge defined by a decoded segment of a length.
   *
   * TODO: Better class name.
   *
   * @param length the length of the segment defined by this Visit
   */
  case class Visit(length: Int) extends Edge

  /**
   * An edge to the failed state. A given `Throwable` was encountered, so no
   * further progress is possible. A state machine implementation may choose
   * whether to reset itself or fail entirely on `Error` transitions.
   */
  case class Error(throwable: Throwable) extends Edge

  /**
   * A state defined by the edges that the machine has traversed.
   *
   * `self` contains the lengths of each ordered, contiguous segment of `buf`
   * that contain the data of tokens representing messages of type `A`.
   */
  sealed trait State extends SeqProxy[Int] {
    /**
     * All data buffered up to the time of this state machine traversal.
     */
    val buf: ByteBuffer
  }

  /**
   * The initial state of any state machine.
   */
  case class InitialState(buf: ByteBuffer) extends State {
    val self = Seq.empty[Int]
  }

  /**
   * An internal, non-terminal state defined by a sequence of
   * previously-traversed edges and a buffer. The reader index of `buf` must be
   * equal to `self.sum`.
   */
  case class NonTerminalState(self: Seq[Int], buf: ByteBuffer) extends State

  /**
   * A terminal state of a given graph traversal. Given a sequence of indices
   * defining segments of `buf`, `builder` will produce an object of type `A`.
   * corresponding segments.
   */
  case class TerminalState[+A](
    self: Seq[Int],
    buf: ByteBuffer,
    builder: (Seq[Int], ByteBuffer) => A
  ) extends State {
    lazy val value: A = builder(self, buf)
  }
}
