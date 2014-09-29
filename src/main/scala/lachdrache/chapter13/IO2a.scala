package lachdrache.chapter13

import lachdrache.chapter11.Monad

object IO2a {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    def printLine(s: String): IO[Unit] =
      Suspend(() => Return(println(s)))

    def unit[A](a: => A): IO[A] =
      Return(a)
    def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] =
      ma flatMap f

    @annotation.tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }

  }

  object ForeverApp extends App {
    import IO._
    val p = forever(printLine("Still going..."))
  }

}
