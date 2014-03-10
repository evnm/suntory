package net.evanmeagher.suntory

import com.twitter.util.Future
import scala.collection.mutable.Buffer

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
        case Nil => Future.Done
        case results if results.nonEmpty => Future.join {
          results map write
        }
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
