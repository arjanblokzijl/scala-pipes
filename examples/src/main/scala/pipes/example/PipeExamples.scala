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

  val list = Stream.from(1).take(10000).toList
  val start = System.currentTimeMillis()
  val fromList = PL.fromList[Int, IO](list)
  val endFromList = System.currentTimeMillis()
  println("fromList: it took %s millisseconds" format (endFromList - start))
//  println("fromList %s" format fromList)
//
  val take = PL.take[Int, IO](1000)
  val endTake = System.currentTimeMillis()
//  println("take %s" format take)
  println("take: it took %s milliseconds" format (endTake - endFromList))

  val takeFromList = take >+> fromList
  val endTakeFromList = System.currentTimeMillis()

  println("takeFromList: it took %s milliseconds" format (endTakeFromList - endTake))
  val printer: Pipe[Zero, Zero, IO, Unit] = fromList <+< take <+< PL.printer[Stream, Int]

  println(runPipe(printer).unsafePerformIO)
}

