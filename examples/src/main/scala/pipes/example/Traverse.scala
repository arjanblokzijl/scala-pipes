package pipes.example

/**
 * User: arjan
 */

import pipes._
import collection.immutable.Stream
import scalaz.std.stream._
import scalaz.std.option._
import scalaz.effect.IO
import scalaz._
import scalaz.State._
import StateT._
import scalaz.effect._

object Traverse extends App {

  def from(o: Option[Int]): Stream[Option[Int]] = Stream.cons(o, from(o.map(i => i + 1)))
  val s: Stream[Option[Int]] = from(Some(1))
////  println("stream: " + s)
//  val res: Option[Stream[Int]] = streamInstance.traverse(s)(i => some(1))
//  println("stream traverse: " + res.map(x => x.take(100).force))

  val as = Stream.range(0, 100000)
  val state: State[Int, IO[Stream[Int]]] = streamInstance.traverseSTrampoline(as)(a => State((s: Int) => (IO(s), a)))
  println("trampoline traverse is %s" format state.eval(0).unsafePerformIO.take(100).force)
}
