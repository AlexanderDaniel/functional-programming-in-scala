package lachdrache.chapter13

import java.util.concurrent.{Future, Executors}

import lachdrache.chapter13.IO3.Console._
import lachdrache.chapter13.IO3._
import lachdrache.chapter7.Par.Par

object RunConsoleParApp extends App {

  def p: ConsoleIO[Unit] = for {
    _ <- printLn("What's your name?")
    n <- readLn
    _ <- n match {
      case Some(n) => printLn(s"Hello, $n!")
      case None => printLn(s"Fine, be that way.")
    }
  } yield ()

  println(p)

  val par: Par[Unit] = runConsolePar(p)

  val future: Future[Unit] = par.apply(Executors.newCachedThreadPool())

  future.get()

}
