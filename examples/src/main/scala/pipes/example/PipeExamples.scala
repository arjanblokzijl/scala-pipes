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

}
