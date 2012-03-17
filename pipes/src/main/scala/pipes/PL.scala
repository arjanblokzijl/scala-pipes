package pipes


import pipes._
import scalaz._

import effect.IO
import Pipe._
/**
 * User: arjan
 */

object PL {

  def printer[F[_], A](implicit M: Monad[F]): Consumer[A, IO, Unit] =
    forever(pipes.await flatMap((x: A) => (pipeMonadTrans[A, Zero].liftM(IO.putStrLn(x.toString)))))

  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, F, Unit] = {
    val aw: Pipe[A, A, F, Unit] = pipes.await[A, A, F] flatMap (x => yieldp[A, A, F](x))
    def go(count: Int, acc: => Pipe[A, A, F, Unit]): Pipe[A, A, F, Unit] =
      if (count == 0) acc
      else (acc flatMap(_ => go(count - 1, acc)))

    go(n - 1, aw)
  }

  def fromStream[A, F[_]](l: => Stream[A])(implicit M: Monad[F]): Producer[A, F, Unit] = l match {
    case Stream.Empty => Pure()
    case x #:: xs => yieldp(x) flatMap(_ => fromStream(xs))
  }
}
