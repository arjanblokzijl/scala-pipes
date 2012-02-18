package pipes
package example

import scalaz.Id

import pipes._
/**
 * User: arjan
 */

object PipeExamples extends App {

  val take = PL.take[Int, Id](5)

  println("take: " + take)

  val id = idP[Int, Id, Unit]

  println("id: " + id)

  val fromList = PL.fromList[Int, Id](Stream(1,2,3,4,5,6))

  println("fromList " + fromList)

  //TODO runPipe, obviously
}
