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

//  val id = idP[Int, Id, Unit]
  val printer = PL.printer[Stream, Int] <+< take <+< PL.fromList(Stream(1,2,3,4,5,6))
  val run = runPipe(printer)

  println("printer " + run)
}
