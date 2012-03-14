package pipes

import scalaz._
import pipes._

/**
 *
 * @tparam A The type of input received from upstream pipes
 * @tparam B The type of output delivered to downstream pipes
 * @tparam F The base monad
 * @tparam R The type of the monad's final result
 */
import scalaz.Free._
import std.function._
import Pipe._

sealed trait Pipe[A, B, F[_], R] {
  def fold[Z](pure: (=> R) => Z
              , mo: (=> F[Pipe[A, B, F, R]]) => Z
              , await: (=> A => Pipe[A, B, F, R]) => Z
              , yieldp: (=> B, => Pipe[A, B, F, R]) => Z): Z

  //define like this to avoid 'type constructor inapplicable to none' compiler errors. This is fixed in scala 2.10
  def map[S](f: (R) => S)(implicit F: Monad[F]): Pipe[A, B, F, S] = flatMap(a => Pure(f(a)))

  def flatMapT[S](f: (R) => Trampoline[Pipe[A, B, F, S]])(implicit F: Monad[F]): Trampoline[Pipe[A, B, F, S]] = fold (
    pure = r => f(r)
    , mo = value => return_(MO(F.map(value)(pi => (pi flatMapT f).run)))
    , await = fc =>  return_(Await.apply(a => (fc.apply(a) flatMapT f).run))
    , yieldp = (b, p) => (p flatMapT f) map(x => Yield(b, x))
  )

  def flatMap[S](f: (R) => Pipe[A, B, F, S])(implicit F: Monad[F]): Pipe[A, B, F, S] = fold(
    pure = r => f(r)
    , mo = value => MO(F.map(value)(pi => pi flatMap f))
    , await = fc => Await(a => fc.apply(a) flatMap f)
    , yieldp = (b, p) => Yield(b, p flatMap f)
  )

   def >+>[C](that: Pipe[C, A, F, R])(implicit M: Monad[F]): Pipe[C, B, F, R] =
      (Lazy(this) compose Lazy(that)) unLazy

   def <+<[C](that: Pipe[B, C, F, R])(implicit M: Monad[F]): Pipe[A, C, F, R] =
      (Lazy(that) compose Lazy(this)) unLazy
}

object Pipe {
  private[this] val ToNone: ((=> Any) => None.type) = x => None
  private[this] val ToNone1: (Any => None.type) = x => None
  private[this] val ToNone2: ((=> Any, => Any) => None.type) = (x, y) => None
  private[this] val ToNone3: ((=> Any, => Any, => Any) => None.type) = (x, y, z) => None

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
    def apply[A, B, F[_], R](fc: A => Pipe[A, B, F, R]) = new Pipe[A, B, F, R] {
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
//  def compose1[C](that: Lazy[F, R, C, A])(implicit M: Monad[F]): Lazy[F, R, C, B] = {
//    def go(p1: Pipe[A, B, F, R], p2: Pipe[C, A, F, R])(implicit M: Monad[F]): Trampoline[Pipe[C, B, F, R]] = (p1, p2) match {
//      case (Yield(x1, p1), p2) => yieldp(x1) flatMapT (_ => go(p1, p2))
//      case (MO(m1), p2) => pipeMonadTrans.liftM(m1) flatMapT ((p1: Pipe[A, B, F, R]) => go(p1, p2))
//      case (Pure(r1), _) => return_(Pure(r1))
//      case (Await(f1), Yield(x2, p2)) => go(f1(x2), p2)
//      case (p1, Await(f2)) => pipes.await[C, B, F] flatMapT (x => go(p1, f2(x)))
//      case (p1, MO(m2)) => pipeMonadTrans.liftM(m2) flatMapT (p2 => go(p1, p2))
//      case (_, Pure(r2)) => return_(Pure(r2))
//    }
//    Lazy(go(this.unLazy, that.unLazy).run)
////    Lazy(p)
//  }


}

case class Strict[F[_], R, A, B](unStrict: Pipe[A, B, F, R])

trait PipeFunctions {
  sealed trait Zero
  object Zero extends Zero

  type Producer[B, F[_], R ] = Pipe[Zero, B, F, R]
  type Pipeline[F[_], R] = Pipe[Zero, Zero, F, R]

  /**
   * Wait for input from the upstream within the Pipe monad.
   * Await blocks until the input is ready.
   * @tparam A
   * @tparam B
   * @tparam F
   * @return
   */
  def await[A, B, F[_]]: Pipe[A, B, F, A] = Await(Pure(_))

  /**
   * Pass the ouptut downstream within the Pipe monad.
   * yield blocks until the output has been received.
   * @param x
   * @tparam A
   * @tparam B
   * @tparam F
   * @return
   */
  def yieldp[A, B, F[_]](x: B): Pipe[A, B, F, Unit] = Yield(x, Pure(()))

  def pipe[A, B, F[_], R](f: A => B)(implicit M: Monad[F]): Pipe[A, B, F, R] =
    forever(await flatMap(x => yieldp(f(x))))

  def discard[A, B, F[_], R](implicit M: Monad[F]): Pipe[A, B, F, R] = forever(await)

  //TODO looks like stack blowing version of forever, specialized to pipes for the moment.
  // Apart from this, all the type annotations are not that great, so generalize
  def forever[A, B, F[_], R, S](fa: Pipe[A, B, F, R])(implicit M: Monad[F]): Pipe[A, B, F, S] = {

    import Free._
    import std.function._
    def go(p: Pipe[A, B, F, R]): Trampoline[Pipe[A, B, F, S]] = {
      p.flatMapT(_ => go(p))
    }
    go(fa).run
  }

  import Free._
  import std.function._
  def foreverT[A, B, F[_], R](fa: Trampoline[Pipe[A, B, F, R]])(implicit M: Monad[F]): Trampoline[Pipe[A, B, F, R]] = {
    fa.flatMap(_ => foreverT(fa))
  }


  def idP[A, F[_], R](implicit M: Monad[F]): Pipe[A, A, F, R] = pipe(identity)

  def runPipe[F[_], R](pl: Pipeline[F, R])(implicit M: Monad[F]): F[R] = pl match {
    case Pure(r) => M.point(r)
    case MO(mp) => M.bind(mp)(runPipe(_))
    case Await(f) => runPipe(f(Zero))
    case Yield(_, p) => runPipe(p)
  }
}

object pipes extends PipeFunctions with PipeInstances