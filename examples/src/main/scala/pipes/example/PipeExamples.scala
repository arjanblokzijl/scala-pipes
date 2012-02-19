package pipes
package example

import scalaz.Id

import pipes._
import collection.immutable.Stream
import scalaz.std.stream._
import scalaz.std.option._
import scalaz.effect.IO

/**
 * User: arjan
 */

object PipeExamples extends App {

  val take = PL.take[Int, IO](100)
  val fromList = PL.fromList[Int, IO](Stream.from(1).take(10000).force)

  val printer: Pipe[Zero, Zero, IO, Unit] = PL.printer[Stream, Int] <+< take <+< fromList
  val run: IO[Unit] = runPipe(printer)

  println(run.unsafePerformIO)
}
