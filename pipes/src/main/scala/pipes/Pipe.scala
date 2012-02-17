package pipes

import scalaz.Monad

/**
 * User: arjan
 */

/**
 *
 * @tparam A The type of input received from upstream pipes
 * @tparam B The type of output delivered to downstream pipes
 * @tparam F The base monad
 * @tparam R The type of the monad's final result
 */
sealed trait Pipe[A, B, F[_], R] {
  //define like this to avoid 'type constructor inapplicable to none' compiler errors. This is fixed in scala 2.10
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
  implicit def pipeMonad[I, O, F[_]](implicit F0: Monad[F]): Monad[({type l[r] = Pipe[I, O, F, r]})#l] = new Monad[({type l[r] = Pipe[I, O, F, r]})#l] {
    def bind[A, B](fa: Pipe[I, O, F, A])(f: (A) => Pipe[I, O, F, B]): Pipe[I, O, F, B] = fa flatMap f

    def point[A](a: => A) = Pure(a)
  }

}