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

  val take = PL.take[Int, IO](5)
  val fromList = PL.fromList[Int, IO](Stream.from(1).take(100000))

  val printer2: Pipe[Zero, Zero, IO, Unit] = PL.printer[Stream, Int] >+> take >+> fromList
  val run2: IO[Unit] = runPipe(printer2)

  println(run2.unsafePerformIO)
}

