package pipes

import scalaz._
import pipes._
import Pipe._

/**
 * A Pipe is a monad transformer with the ability to 'await' input from or 'yield' output to other Pipes.
 * Pipes resemble enumeratees in the Iteratee libraries since they receive an input stream and transform it
 * into a new stream.
 * What distinguishes 'Pipe's from every other iteratee implementation is that
 * they form a 'Category'.  Because of this, 'Pipe's can be composed into 'Pipeline's.
 *
 * @tparam A The type of input received from upstream pipes
 * @tparam B The type of output delivered to downstream pipes
 * @tparam F The base monad
 * @tparam R The type of the monad's final result
 */
sealed trait Pipe[A, B, F[_], R] {
  def fold[Z](pure: (=> R) => Z
              , mo: (=> F[Pipe[A, B, F, R]]) => Z
              , await: (=> A => Pipe[A, B, F, R]) => Z
              , yieldp: (=> B, => Pipe[A, B, F, R]) => Z): Z

  def map[S](f: (R) => S)(implicit F: Monad[F]): Pipe[A, B, F, S] = flatMap(a => Pure(f(a)))

  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]): Pipe[A, B, F, S] = {
    def through(p: Pipe[A, B, F, R]):  Pipe[A, B, F, S] = p.fold(
      pure = r => f(r)
      , mo = value => MO(F.map(value)(pi => through(pi)))
      , await = fc => Await(a => through(fc.apply(a)))
      , yieldp = (b, p) => Yield(b, through(p))
    )
    through(this)
  }

  def foldr[S](z: => Pipe[A, B, F, S])(f: (R, => Pipe[A, B, F, S]) => Pipe[A, B, F, S])(implicit F: Monad[F]): Pipe[A, B, F, S] = {
    def go(xs: => Pipe[A, B, F, R], f: (R, => Pipe[A, B, F, S]) => Pipe[A, B, F, S]): Pipe[A, B, F, S] = xs.fold(
      pure = r => f(r, z)
      , mo = value => MO(F.map(value)(pi => go(pi, f)))
      , await = fc => Await(a => go(fc.apply(a), f))
      , yieldp = (b, p) => Yield(b, go(p, f))
    )
    go(this, f)
  }

   def >+>[C](that: Pipe[C, A, F, R])(implicit M: Monad[F]): Pipe[C, B, F, R] =
      (Lazy(this) compose Lazy(that)) unLazy

    def <+<[C](that: Pipe[B, C, F, R])(implicit M: Monad[F]): Pipe[A, C, F, R] =
      (Lazy(that) compose Lazy(this)) unLazy

   def >->[C](that: Pipe[C, A, F, R])(implicit M: Monad[F]): Pipe[C, B, F, R] =
      (Strict(this) compose Strict(that)) unStrict

    def <-<[C](that: Pipe[B, C, F, R])(implicit M: Monad[F]): Pipe[A, C, F, R] =
      (Strict(that) compose Strict(this)) unStrict
}

object Pipe {
  private[this] val ToNone1: (Any => None.type) = x => None
  private[this] val ToNone2: ((=> Any, => Any) => None.type) = (x, y) => None

  object Pure {
    def apply[A, B, F[_], R](r: => R) = new Pipe[A, B, F, R] {
      def fold[Z](pure: (=> R) => Z
                  , mo: (=> F[Pipe[A, B, F, R]]) => Z
                  , await: (=> A => Pipe[A, B, F, R]) => Z
                  , yieldp: (=> B, => Pipe[A, B, F, R]) => Z) = pure(r)
    }
    def unapply[A, B, F[_], R](p: Pipe[A, B, F, R]): Option[R] = {
      p.fold(r => Some(r), ToNone1, ToNone1, ToNone2)
    }
  }

  object Yield {
    def apply[A, B, F[_], R](b: => B, p: => Pipe[A, B, F, R]) = new Pipe[A, B, F, R] {
      def fold[Z](pure: (=> R) => Z
                  , mo: (=> F[Pipe[A, B, F, R]]) => Z
                  , await: (=> A => Pipe[A, B, F, R]) => Z
                  , yieldp: (=> B, => Pipe[A, B, F, R]) => Z) = yieldp(b, p)
    }
    def unapply[A, B, F[_], R](p: Pipe[A, B, F, R]): Option[(B, Pipe[A, B, F, R])] = {
      p.fold(ToNone1, ToNone1, ToNone1, (b, p) => Some(b, p))
    }
  }

  object Await {
    def apply[A, B, F[_], R](fc: => A => Pipe[A, B, F, R]) = new Pipe[A, B, F, R] {
      def fold[Z](pure: (=> R) => Z
                  , mo: (=> F[Pipe[A, B, F, R]]) => Z
                  , await: (=> A => Pipe[A, B, F, R]) => Z
                  , yieldp: (=> B, => Pipe[A, B, F, R]) => Z) = await(fc)
    }
    def unapply[A, B, F[_], R](p: Pipe[A, B, F, R]): Option[A => Pipe[A, B, F, R]] = {
      p.fold(ToNone1, ToNone1, f => Some(f), ToNone2)
    }
  }

  object MO {
    def apply[A, B, F[_], R](value: => F[Pipe[A, B, F, R]]) = new Pipe[A, B, F, R] {
      def fold[Z](pure: (=> R) => Z
                  , mo: (=> F[Pipe[A, B, F, R]]) => Z
                  , await: (=> A => Pipe[A, B, F, R]) => Z
                  , yieldp: (=> B, => Pipe[A, B, F, R]) => Z) = mo(value)
    }
    def unapply[A, B, F[_], R](p: Pipe[A, B, F, R]): Option[F[Pipe[A, B, F, R]]] = {
      p.fold(ToNone1, f => Some(f), ToNone1, ToNone2)
    }

  }
}

trait PipeInstances {
  implicit def pipeMonad[I, O, F[_]](implicit F0: Monad[F]): Monad[({type l[r] = Pipe[I, O, F, r]})#l] = new Monad[({type l[r] = Pipe[I, O, F, r]})#l] {
    def bind[A, B](fa: Pipe[I, O, F, A])(f: (A) => Pipe[I, O, F, B]): Pipe[I, O, F, B] = fa flatMap f

    def point[A](a: => A) = Pure(a)
  }

  implicit def pipeMonadTrans[I, O]: MonadTrans[({type l[a[_], b] = Pipe[I, O, a, b]})#l] = new MonadTrans[({type l[a[_], b] = Pipe[I, O, a, b]})#l] {
    implicit def apply[M[_]](implicit M0: Monad[M]): Monad[({type l[a] = Pipe[I, O, M, a]})#l] = pipeMonad[I, O, M]
    def liftM[G[_], A](ga: G[A])(implicit M: Monad[G]): Pipe[I, O, G, A] = MO(M.map(ga)(a => pipeMonad[I, O, G].point(a)))
  }

  implicit def pipeCategory[F[_], R](implicit M: Monad[F]): Category[({type l[a, b] = Lazy[F, R, a, b]})#l] = new Category[({type l[a, b] = Lazy[F, R, a, b]})#l] {
    def compose[A, B, C](f: Lazy[F, R, B, C], g: Lazy[F, R, A, B]): Lazy[F, R, A, C] = f compose g
    def id[A]: Lazy[F, R, A, A] = Lazy(pipes.idP)
  }
}

/**
 * A Category instance with lazy semantics, meaning each pipe consumes as little input as possible.
 * The Pipe control flow rules for lazy composition are as follows:
 *
 * Execution begins at the most downstream 'Pipe'.

 * $ - If a 'Pipe' 'await's input, it blocks and transfers control to the next
 * 'Pipe' upstream until that 'Pipe' 'yield's back a value.
 *
 * $ - If a 'Pipe' 'yield's output, it restores control to the original
 * downstream 'Pipe' that was 'await'ing its input and binds its result to
 * the return value of the 'await' command.
 *
 * $- If a 'Pipe' terminates, it terminates every other 'Pipe' composed with it.
 *
 * The last rule follows from laziness.  If a 'Pipe' terminates then every
 * downstream 'Pipe' depending on its output cannot proceed, and upstream
 * 'Pipe's are never evaluated because the terminated 'Pipe' will not request values from them any longer.
 *
 */
case class Lazy[F[_], R, A, B](unLazy: Pipe[A, B, F, R]) {
  def compose[C](that: Lazy[F, R, C, A])(implicit M: Monad[F]): Lazy[F, R, C, B] = {
    val p: Pipe[C, B, F, R] = (this.unLazy, that.unLazy) match {
      case (Yield(x1, p1), p2) => yieldp(x1) flatMap (_ => p1 >+> p2)
      case (MO(m1), p2) => pipeMonadTrans.liftM(m1) flatMap ((p1: Pipe[A, B, F, R]) => p1 >+> p2)
      case (Pure(r1), _) => Pure(r1)
      case (Await(f1), Yield(x2, p2)) => f1(x2) >+> p2
      case (p1, Await(f2)) => pipes.await[C, B, F] flatMap (x => p1 >+> f2(x))
      case (p1, MO(m2)) => pipeMonadTrans.liftM(m2) flatMap (p2 => p1 >+> p2)
      case (_, Pure(r2)) => Pure(r2)
    }
    Lazy(p)
  }
}

case class Strict[F[_], R, A, B](unStrict: Pipe[A, B, F, R]) {
  def compose[C](that: Strict[F, R, C, A])(implicit M: Monad[F]): Strict[F, R, C, B] =
    Strict((this.unStrict flatMap(_ => discard[A, B, F, R])) >+> that.unStrict)
}

trait PipeFunctions {
  sealed trait Zero
  object Zero extends Zero

  /**A one way Pipe that only delivers output, making it suitable for the first stage in a pipeline.*/
  type Producer[B, F[_], R ] = Pipe[Zero, B, F, R]

  /**A Pipe that never delivers output downstream, but only consumes input.*/
  type Consumer[A, F[_], R ] = Pipe[A, Zero, F, R]

  /**
   * A final Pipe that is blocked on both ends.
   * It will never 'await' any input and never 'yield' any output.
   */
  type Pipeline[F[_], R] = Pipe[Zero, Zero, F, R]

  /**
   * Wait for input from the upstream within the Pipe monad.
   * Await blocks until the input is ready.
   */
  def await[A, B, F[_]]: Pipe[A, B, F, A] = Await(Pure(_))

  /**
   * Pass the ouptut downstream within the Pipe monad.
   * yield blocks until the output has been received.
   */
  def yieldp[A, B, F[_]](x: B): Pipe[A, B, F, Unit] = Yield(x, Pure(()))

  def pipe[A, B, F[_], R](f: A => B)(implicit M: Monad[F]): Pipe[A, B, F, R] =
    forever(await flatMap(x => yieldp(f(x))))

  def discard[A, B, F[_], R](implicit M: Monad[F]): Pipe[A, B, F, R] = forever(await)

  def forever[A, B, F[_], R, S](fa: Pipe[A, B, F, R])(implicit M: Monad[F]): Pipe[A, B, F, S] =
    fa.flatMap(_ => forever(fa))

  def idP[A, F[_], R](implicit M: Monad[F]): Pipe[A, A, F, R] = pipe(identity)

  def runPipe[F[_], R](pl: Pipeline[F, R])(implicit M: Monad[F]): F[R] = pl match {
    case Pure(r) => M.point(r)
    case MO(mp) => M.bind(mp)(runPipe(_))
    case Await(f) => runPipe(f(Zero))
    case Yield(_, p) => runPipe(p)
  }
}

object pipes extends PipeFunctions with PipeInstances