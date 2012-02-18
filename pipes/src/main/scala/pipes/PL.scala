package pipes


import pipes._
import scalaz._

import effect.IO
import std.stream._
import collection.immutable.Stream

/**
 * User: arjan
 */

object PL {

//  > printer :: (Show a) => Pipe a Zero IO b
//  > printer = forever $ do
//  >     x <- await
//  >     lift $ print x
  def printer[F[_], A](implicit M: Monad[F]): Pipe[A, Zero, IO, Unit] = {
    pipeMonad[A, Zero, IO].bind(await)((x: A) => pipeMonadTrans[A, Zero].liftM(IO.putStrLn(x.toString)))
  }


  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, F, Unit] = {
    type Res = Pipe[A, A, F, Unit]
    val pm = pipeMonad[A, A, F].bind(await)(x => yieldp(x))
    replicate[A, A, F, Unit](n, pm)
  }

  def fromList[A, F[_]](l: Stream[A])(implicit M: Monad[F]): Pipe[Zero, A, F, Unit] = {
    val lm = l map(x => yieldp[Zero, A, F](x))
    seqp[Zero, A, F](lm)
  }

  def replicate[A, B, F[_], R](n: Int, pm: Pipe[A, B, F, Unit])(implicit M: Monad[F]): Pipe[A, B, F, Unit] = {
    type Res = Pipe[A, B, F, Unit]
    Monoid.replicate[Stream, Res](pm)(n).foldLeft[Res](Pure(()))((b, fa) => pipeMonad[A, B, F].bind(fa)(_ => Pure(b)))
  }

  def seqp[A, B, F[_]](str: Stream[Pipe[A, B, F, Unit]])(implicit M: Monad[F]): Pipe[A, B, F, Unit] = {
    type Res = Pipe[A, B, F, Unit]
    str.foldLeft[Res](Pure(()))((b, fa) => pipeMonad[A, B, F].bind(fa)(_ => Pure(b)))
  }

}
