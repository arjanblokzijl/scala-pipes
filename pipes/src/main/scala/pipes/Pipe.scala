package pipes

import scalaz.Monad

/**
 * User: arjan
 */

//data Pipe a b m r =
//    Pure r                     -- pure = Pure
//  | M     (m   (Pipe a b m r)) -- Monad
//  | Await (a -> Pipe a b m r ) -- ((->) a) Functor
//  | Yield (b,   Pipe a b m r ) -- ((,)  b) Functor

//instance (Monad m) => Functor (Pipe a b m) where
//    fmap f c = case c of
//        Pure r   -> Pure $ f r
//        M mc     -> M     $ liftM (fmap f) mc
//        Await fc -> Await $ fmap  (fmap f) fc
//        Yield fc -> Yield $ fmap  (fmap f) fc

sealed trait Pipe[A, B, F[_], R] {
  def map[S](f: (R) => S)(implicit F: Monad[F]): Pipe[A, B, F, S]
  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]): Pipe[A, B, F, S]
}
case class Pure[A, B, F[_], R](r: R) extends Pipe[A, B, F, R] {
  def map[S](f: (R) => S)(implicit F: Monad[F]) = Pure(f(r))
  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]) = f(r)
}
case class M[A, B, F[_], R](value: F[Pipe[A, B, F, R]]) extends Pipe[A, B, F, R] {
  def map[S](f: (R) => S)(implicit F: Monad[F]) = M(F.map(value)(p => p map f))

  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]) = M(F.map(value)(pi => pi flatMap f))
}
case class Await[A, B, F[_], R](fc: A => Pipe[A, B, F, R]) extends Pipe[A, B, F, R] {
  def map[S](f: (R) => S)(implicit F: Monad[F]) = Await(a => fc(a) map f)
  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]) = Await(a => fc(a) flatMap f)
}
case class Yield[A, B, F[_], R](b: B, p: Pipe[A, B, F, R]) extends Pipe[A, B, F, R] {
  def map[S](f: (R) => S)(implicit F: Monad[F]) = Yield(b, p map f)
  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]) = Yield(b, p flatMap f)
}

trait PipeFunctions {
//  instance (Monad m) => Monad (Pipe a b m) where
//      return = pure
//      m >>= f = case m of
//          Pure r   -> f r
//          M mc     -> M     $ liftM (>>= f) mc
//          Await fc -> Await $ fmap  (>>= f) fc
//          Yield fc -> Yield $ fmap  (>>= f) fc

  implicit def pipeMonad[I, O, F[_]](implicit F0: Monad[F]): Monad[({type l[r] = Pipe[I, O, F, r]})#l] = new Monad[({type l[r] = Pipe[I, O, F, r]})#l] {
    def bind[A, B](fa: Pipe[I, O, F, A])(f: (A) => Pipe[I, O, F, B]): Pipe[I, O, F, B] = fa flatMap f

    def point[A](a: => A) = Pure(a)
  }

}