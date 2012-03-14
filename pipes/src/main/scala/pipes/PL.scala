package pipes


import pipes._
import scalaz._

import effect.IO
import std.list._
import collection.Seq

import Pipe._
/**
 * User: arjan
 */

object PL {

  def printer[F[_], A](implicit M: Monad[F]): Pipe[A, Zero, IO, Unit] = {
    import Free._
    import std.function._
    val await: Pipe[A, Zero, IO, A] = pipes.await
    val print: Free.Trampoline[Pipe[A, Zero, IO, Unit]] = await flatMapT((x: A) => return_(pipeMonadTrans[A, Zero].liftM(IO.putStrLn(x.toString))))
    forever(print.run)
  }

  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, F, Unit] = {
    val dl = DList.replicate(n, pipes.await[A, A, F] flatMap (x => yieldp[A, A, F](x)))
    dl.foldr[Pipe[A, A, F, Unit]](Pure())((a, b) => b flatMap(_ => a))
  }

  def fromList[A, F[_]](l: List[A])(implicit M: Monad[F]): Pipe[Zero, A, F, Unit] =
    DList.fromList(l).foldr[Pipe[Zero, A, F, Unit]](Pure())((a, b) => yieldp(a) flatMap(_ => b))


  def replicate[A, B, F[_], R](n: Int, pm: Pipe[A, B, F, Unit])(implicit M: Monad[F]): Pipe[A, B, F, Unit] =
    seqp(Monoid.replicate[List, Pipe[A, B, F, Unit]](pm)(n))

  //TODO this is strict, blows up on infinite streams
  def seqp[A, B, F[_]](str: Seq[Pipe[A, B, F, Unit]])(implicit M: Monad[F]): Pipe[A, B, F, Unit] = {
    str.foldLeft[Pipe[A, B, F, Unit]](Pure(()))((b, a) => {
      b flatMap(_ => a)
    })
  }
}
