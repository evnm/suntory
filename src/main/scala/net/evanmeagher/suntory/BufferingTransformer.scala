package net.evanmeagher.suntory

import com.twitter.util.Future
import scala.collection.mutable.{ArrayBuffer, Buffer}

/**
 * A buffered data transformation. Input passed to `apply` is buffered until the
 * conditions of `consume` are satisfied, at which point the buffered input is
 * consumed.
 *
 * @tparam A The input type of this transformer
 * @tparam B The output type of this transformer
 * @param consume A PartialFunction mapping sequences of the input type A to a
 * tuple of the produced B value and remainder sequence of input
 */
case class BufferingTransformer[A: ClassManifest, B](
  consume: PartialFunction[Seq[A], (B, Seq[A])])
{ self =>
  // Empty constructor for use by `andThen`. Null PartialFunction copied from
  // Scala 2.10.3 source because 2.9.3 has no `PartialFunction#empty`.
  private[this] val empty_pf = new PartialFunction[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
  }

  // TODO: Mitigate unbounded queueing.
  private[this] val buf = new ArrayBuffer[A]

  /**
   * On input consumption, the buffer is shifted accordingly and results
   * are returned.
   */
  def apply(input: Iterable[A]): Option[B] = {
    buf ++= input

    // TODO: Support multiple `consume` emissions per application?
    if (consume.isDefinedAt(buf)) {
      val (result, remainder) = consume(buf)
      buf.clear()
      buf ++= remainder
      Some(result)
    } else {
      None
    }
  }

  def apply(input: A): Option[B] = apply(Seq(input))

  /**
   * Compose this BufferingTransformer with another whose input type matches
   * `this`s output type.
   */
  def andThen[C](other: BufferingTransformer[B, C]): BufferingTransformer[A, C] = {
    new BufferingTransformer[A, C](empty_pf) {
      override def apply(input: A): Option[C] =
        self.apply(input) flatMap other.apply

      override def reset() {
        self.reset()
        other.reset()
      }
    }
  }

  def reset() {
    buf.clear()
  }
}

/**
 * An adapter for connecting two async read and write interfaces together with
 * a [[net.evanmeagher.suntory.BufferingTransformer BufferingTransformer]].
 */
trait Adapter[A, B] {
  val transformer: BufferingTransformer[A, B]
  def read(): Future[A]
  def write(value: B): Future[Unit]

  private[this] def loop(): Future[Unit] = {
    read() flatMap { input =>
      (transformer(input) match {
        case Some(value) => write(value)
        case _ => Future.Done
      }) before loop()
    }
  }
  loop()
}

/**
 * An [[net.evanmeagher.suntory.Adapter Adapter] that reads input from a static
 * Buffer and writes output to Console.out.
 */
class ConsoleAdapter[A, B](
    val transformer: BufferingTransformer[A, B],
    input: Buffer[A])
  extends Adapter[A, B]
{
  def read() = Future.value(input.remove(0))

  def write(value: B) = {
    println(value)
    Future.Done
  }
}
