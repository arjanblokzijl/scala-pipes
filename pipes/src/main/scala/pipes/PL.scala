package pipes


import pipes._
import scalaz._

import effect.IO
import std.stream._
import std.list._
import std.stream.streamSyntax._
import collection.immutable.Stream

/**
 * User: arjan
 */

object PL {

  def printer[F[_], A](implicit M: Monad[F]): Pipe[A, Zero, IO, Unit] =
    forever(pipeMonad[A, Zero, IO].bind(await)((x: A) =>
      pipeMonadTrans[A, Zero].liftM(IO.putStrLn(x.toString))))

  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, F, Unit] =
    replicate[A, A, F, Unit](n, await[A, A, F] flatMap (x => yieldp[A, A, F](x)))

  def fromList[A, F[_]](l: Seq[A])(implicit M: Monad[F]): Pipe[Zero, A, F, Unit] =
    seqp[Zero, A, F](l map yieldp[Zero, A, F])

  def replicate[A, B, F[_], R](n: Int, pm: Pipe[A, B, F, Unit])(implicit M: Monad[F]): Pipe[A, B, F, Unit] =
    seqp(Monoid.replicate[List, Pipe[A, B, F, Unit]](pm)(n))

  //TODO this is strict, blows up on infinite streams
  def seqp[A, B, F[_]](str: Seq[Pipe[A, B, F, Unit]])(implicit M: Monad[F]): Pipe[A, B, F, Unit] =
    str.foldLeft[Pipe[A, B, F, Unit]](Pure(()))((b, fa) => b flatMap(_ => fa))
}
