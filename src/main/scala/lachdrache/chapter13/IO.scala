package lachdrache.chapter13

import lachdrache.chapter11.Monad

import scala.io.StdIn

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run: B = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run: B = f(self.run).run }
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] =
    new IO[A] { def run: A = a }
  override def flatMap[A,B](ma: IO[A])(f: A => IO[B]): IO[B] =
    ma flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def ReadLine: IO[String] = IO { StdIn.readLine() }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  val echo: IO[Unit] = ReadLine.flatMap(PrintLine)
  val readInt: IO[Int] = ReadLine.map(_.toInt)
  val readInts: IO[(Int, Int)] = IO.product(readInt, readInt)
}
