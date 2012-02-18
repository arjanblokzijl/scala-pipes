package pipes


import pipes._
import scalaz._
import std.stream._

/**
 * User: arjan
 */

object PL {


  //TODO should be generalized a bit
  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, Stream, Unit] = {
    type Res = Pipe[A, A, Stream, Unit]
    val pm = pipeMonad[A, A, Stream].bind(await[A, A, Stream])(x => yieldp(x))
    Monoid.replicate[Stream, Res](pm)(n).foldLeft[Res](Pure(()))((b, fa) => pipeMonad[A, A, Stream].bind(fa)(_ => Pure(b)))
  }

//  def replicateM_[F[_], A](n: Int)(fa: F[A])(implicit M: Monad[F]): F[Unit] = {
//    val r = Monoid.replicate[Stream, F[A]](fa)(n)
//    sequence_(r)
//  }
//
//  def sequence_[F[_], A](ms: Stream[F[A]])(implicit M: Monad[F]): F[Unit] = {
//    ms.foldLeft(M.point(()))((b, fa) => M.bind(fa)(_ => M.point(b)))
//  }
}
