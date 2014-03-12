package net.evanmeagher.suntory

import java.nio.charset.Charset
import scala.collection.mutable.ArrayBuffer

object BufferingTransformer {
  // Empty constructor for use by `andThen`. Null PartialFunction copied from
  // Scala 2.10.3 source because 2.9.3 has no `PartialFunction#empty`.
  private[suntory] val empty_pf = new PartialFunction[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
  }

  /**
   * A no-op BufferingTransformer.
   */
  def identity[A: ClassManifest] = new BufferingTransformer[A, A]({
    case input => (input, Seq.empty)
  })
}

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
class BufferingTransformer[A: ClassManifest, B](
  consume: PartialFunction[Seq[A], (Seq[B], Seq[A])])
{ self =>
  // TODO: Mitigate unbounded queueing.
  private[this] val buf = new ArrayBuffer[A]

  /**
   * On input consumption, the buffer is shifted accordingly and results
   * are returned.
   */
  def apply(input: Seq[A]): Seq[B] = {
    buf ++= input

    // TODO: Support multiple `consume` emissions per application?
    if (consume.isDefinedAt(buf)) {
      // Make a copy of `buf` in order to prevent `consume` from modifying it.
      val dup = new Array[A](buf.size)
      buf.copyToArray(dup)
      val (result, remainder) = consume(dup)
      buf.clear()
      buf ++= remainder
      result
    } else {
      Seq.empty
    }
  }

  def apply(input: A): Seq[B] = apply(Seq(input))

  /**
   * Compose this BufferingTransformer with another whose input type matches
   * `this`s output type.
   */
  def andThen[C](other: BufferingTransformer[B, C]): BufferingTransformer[A, C] = {
    new BufferingTransformer[A, C](BufferingTransformer.empty_pf) {
      override def apply(input: Seq[A]): Seq[C] =
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
 * A [[net.evanmeagher.suntory.BufferingTransformer BufferingTransformer]] that
 * converts byte arrays to strings according to a given character set encoding.
 */
class CharEncodingByteTransformer(charset: Charset)
  extends BufferingTransformer[Byte, Char]({
    case input => (new String(input.toArray, charset), Seq.empty)
  })
{
  def this(charsetName: String) = this(Charset.forName(charsetName))
}

/**
 * A [[net.evanmeagher.suntory.BufferingTransformer BufferingTransformer]] that
 * frames input based on a delimiter of the input type `A`.
 *
 * TODO: Seq and multiple delimiters, e.g. in order to support "\r\n" newlines.
 */
class DelimitingTransformer[A: ClassManifest](delimiter: A)
  extends BufferingTransformer[A, A]({
    case input if input.contains(delimiter) =>
      val (output, remainder) = input.splitAt(input.indexOf(delimiter))
      (output, remainder.tail)
  })

/**
 * A [[net.evanmeagher.suntory.DelimitingTransformer DelimitingTransformer]]
 * that splits a byte stream on encoded newline characters.
 */
class LineReadingByteTransformer extends DelimitingTransformer[Byte]('\n'.toByte)
