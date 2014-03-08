package net.evanmeagher.suntory

import scala.collection.SeqProxy

/**
 * A base trait for state machine transitions.
 */
sealed abstract class Transition[+A]

/**
 * A self-loop. No consumable input, so we remain at the current state.
 */
case object Loop extends Transition[Nothing]

/**
 * Traverse an edge to a given [[net.evanmeagher.suntory.State State]].
 */
case class GoTo[A](destination: State[A]) extends Transition[A]

/**
 * A state defined by the edges that the machine has traversed.
 *
 * `self` contains the values along the path traveled to the given State.
 */
sealed abstract class State[+A] extends SeqProxy[A]

/**
 * The initial state of any state machine.
 */
case object InitialState extends State[Nothing] {
  val self = Seq.empty[Nothing]
}

/**
 * A failed state. A given `Throwable` was encountered, so no further progress
 * is possible. A state machine implementation may choose whether to reset
 * itself or fail entirely when an `Error` state is reached.
 */
case class Error(throwable: Throwable) extends State[Nothing] {
  val self = Seq.empty[Nothing]
}

/**
 * A [[net.evanmeagher.suntory.State State]] that buffers input. Used to define
 * state machines that consume input incrementally until a production condition
 * is met before transitioning to a [[net.evanmeagher.suntory.ProducerState]].
 */
case class BufferState[A](self: Seq[A]) extends State[A]

/**
 * A [[net.evanmeagher.suntory.State State]] that produces a value of type `B`.
 * Output is based on a sequence of * values of type `A` defined by the path taken to arrive at this state.
 */
abstract class ProducerState[A, B](
  self: Seq[A],
  builder: Seq[A] => B
) extends State[A] {
  lazy val value: B = builder(self)
  override lazy val toString = "ProducerState(%s)".format(value)
}

/**
 * A state machine defined by a state transition table.
 */
class Automaton[A, B](
  stateTable: PartialFunction[(State[A], A), Transition[A]]
) {
  private[this] val liftedStateTable = stateTable.lift
  private[this] var currentState: State[A] = InitialState

  def apply(input: A): State[A] = {
    liftedStateTable(currentState, input) match {
      case Some(Loop) => currentState

      case Some(GoTo(bufState@BufferState(self))) =>
        currentState = BufferState(self :+ input)

      case Some(other) =>
        // TODO
        other

      case None =>
        currentState = Error(new IllegalStateException(
          "No transition possible for state=%s and input=%s".format(currentState, input)
        ))
    }

    currentState
  }

  def reset() {
    currentState = InitialState
  }
}
