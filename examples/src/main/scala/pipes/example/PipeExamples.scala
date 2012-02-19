package pipes
package example

import scalaz.Id

import pipes._
import collection.immutable.Stream
import scalaz.std.stream._
import scalaz.effect.IO

/**
 * User: arjan
 */

object PipeExamples extends App {

  val take = PL.take[Int, IO](5)
  val fromList = PL.fromList[Int, IO]((1 to 10).toStream)

  val printer: Pipe[Zero, Zero, IO, Unit] = PL.printer[Stream, Int] <+< take <+< fromList
  val run: IO[Unit] = runPipe(printer)

  println(run.unsafePerformIO)
}
