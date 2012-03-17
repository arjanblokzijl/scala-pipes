package pipes
package example

import pipes._
import collection.immutable.Stream
import scalaz.effect.IO
import scalaz.std.stream._

/**
 * User: arjan
 */

object PipeExamples extends App {
  val str = Stream.from(1)

  val producer = PL.fromStream[Int, IO](str)

  val take10 = PL.take[Int, IO](10)
  val take5 = PL.take[Int, IO](5)
  val largeTake = PL.take[Int, IO](1000000)

  val printer: Pipe[Zero, Zero, IO, Unit] = producer <+< take10 <+< PL.printer[Stream, Int]

  val largePrinter: Pipe[Zero, Zero, IO, Unit] = producer <+< largeTake <+< PL.printer[Stream, Int]

  take10 flatMap (_ => take5)

  println("printing")
  println(runPipe(printer).unsafePerformIO)

  println("composing pipelines ")
  println(runPipe(printer flatMap (_ => printer)).unsafePerformIO)

  println("large pipeline")
  println(runPipe(largePrinter).unsafePerformIO)
}

