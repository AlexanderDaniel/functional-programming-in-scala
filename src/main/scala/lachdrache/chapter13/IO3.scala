package lachdrache.chapter13

import lachdrache.chapter11.Monad
import lachdrache.chapter7.Par
import lachdrache.chapter7.Par.Par

import scala.io.StdIn
import scala.language.higherKinds

/**
 * We can generalize `TailRec` and `Async` to the type `Free`, which is
 * a `Monad` for any choice of `F`. */
object IO3 {

  sealed trait Free[F[_], A] {
    // Exercise 13.1 part 1
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      this flatMap (a => Return(f(a))) // same as `f andThen (Return(_))`
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  // Exercise 13.1 part 2
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      def unit[A](a: => A): Free[F, A] =
        Return(a)
      def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] =
        ma flatMap f
    }

  // Exercise 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(t, g) => runTrampoline(t flatMap (a => g(a) flatMap f))
    }
  }

  // Exercise 13.3
  /** return either a `Suspend`, a `Return`, or a right-associated `FlatMap` */
  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }
  @annotation.tailrec
  def step[F[_], A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
    def toReader: ConsoleReader[A]
    def toState: ConsoleState[A]
  }
  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)
    override def toThunk: () => Option[String] = () => run
    override def toReader = ConsoleReader { in => Some(in) }
    override def toState = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }

    def run: Option[String] =
      try Some(StdIn.readLine())
      catch { case e: Exception => None}
  }
  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))
    override def toThunk: () => Unit = () => println(line)
    override def toReader = ConsoleReader { s => () } // noop
    override def toState = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) } // append to the output

  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  val f1: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console.")
    ln <- Console.readLn
  } yield ln

  /** Translate between any `F[A` to `G[A]` */
  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  /** gives us infix syntax `F ~> G` for `Translate[F,G]` */
  type ~>[F[_], G[_]] = Translate[F,G]

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](a: Console[A]): () => A = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible because `step` eliminates these cases")
    }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)
  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  // Exercise 13.4
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    runTrampoline { translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }
  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)
      override def flatMap[A, B](ma: ConsoleReader[A])(f: (A) => ConsoleReader[B]): ConsoleReader[B] = ma flatMap f
    }
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    override def apply[A](a: Console[A]): ConsoleReader[A] = a.toReader
  }

  def runConsoleReader[A](io: Console.ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A](io)(consoleToReader)

  case class Buffers(in: List[String], out: Vector[String])

  // A specialized state monad
  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }
  }
  object ConsoleState {
    implicit val monad = new Monad[ConsoleState] {
      def unit[A](a: => A) = ConsoleState(bufs => (a,bufs))
      def flatMap[A,B](ra: ConsoleState[A])(f: A => ConsoleState[B]) = ra flatMap f
    }
  }

  val consoleToState =
    new (Console ~> ConsoleState) { def apply[A](a: Console[A]) = a.toState }

  def runConsoleState[A](io: Console.ConsoleIO[A]): ConsoleState[A] =
    runFree[Console,ConsoleState,A](io)(consoleToState)

}
