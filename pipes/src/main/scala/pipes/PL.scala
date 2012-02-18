package pipes


import pipes._
import scalaz._
import std.stream._
import collection.immutable.Stream

/**
 * User: arjan
 */

object PL {


  //TODO should be generalized
  def take[A, F[_]](n: Int)(implicit M: Monad[F]): Pipe[A, A, Stream, Unit] = {
    type Res = Pipe[A, A, Stream, Unit]
    val pm = pipeMonad[A, A, Stream].bind(await)(x => yieldp(x))
    replicate[A, A, Stream, Unit](n, pm)
  }

  def fromList[B, F[_]](l: Stream[B])(implicit M: Monad[F]): Pipe[Zero, B, Stream, Unit] = {
    val lm = l map(x => yieldp[Zero, B, Stream](x))
    seqp[Zero, B, Stream](lm)
  }

  def replicate[A, B, F[_], R](n: Int, pm: Pipe[A, B, Stream, Unit])(implicit M: Monad[F]): Pipe[A, B, Stream, Unit] = {
    type Res = Pipe[A, B, Stream, Unit]
    Monoid.replicate[Stream, Res](pm)(n).foldLeft[Res](Pure(()))((b, fa) => pipeMonad[A, B, Stream].bind(fa)(_ => Pure(b)))
  }

  def seqp[A, B, F[_]](str: Stream[Pipe[A, B, Stream, Unit]])(implicit M: Monad[F]): Pipe[A, B, Stream, Unit] = {
    type Res = Pipe[A, B, Stream, Unit]
    str.foldLeft[Res](Pure(()))((b, fa) => pipeMonad[A, B, Stream].bind(fa)(_ => Pure(b)))
  }

//  -- | @'mapM_' f@ is equivalent to @'sequence_' . 'map' f@.
//  mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
//  {-# INLINE mapM_ #-}
//  mapM_ f as      =  sequence_ (map f as)

//  def replicateM_[F[_], A](n: Int)(fa: F[A])(implicit M: Monad[F]): F[Unit] = {
//    val r = Monoid.replicate[Stream, F[A]](fa)(n)
//    sequence_(r)
//  }
//
//  def sequence_[F[_], A](ms: Stream[F[A]])(implicit M: Monad[F]): F[Unit] = {
//    ms.foldLeft(M.point(()))((b, fa) => M.bind(fa)(_ => M.point(b)))
//  }
}
