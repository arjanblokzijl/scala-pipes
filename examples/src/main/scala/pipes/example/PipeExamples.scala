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

  val smallTake = PL.take[Int, IO](10)
  val largeTake = PL.take[Int, IO](1000000)


  val smallPrint: Pipe[Zero, Zero, IO, Unit] = producer <+< smallTake <+< PL.printer[Stream, Int]

  val largePrint: Pipe[Zero, Zero, IO, Unit] = producer <+< largeTake <+< PL.printer[Stream, Int]

  println("start printing small pipeline")

  println(runPipe(smallPrint).unsafePerformIO)

  println("start printing large pipeline")

  println(runPipe(largePrint).unsafePerformIO)
}

