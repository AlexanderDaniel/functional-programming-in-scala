package lachdrache.chapter13

import lachdrache.chapter13.IO3._

object ConsoleIOApp extends App {

  val justConsoleIo: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console.")
    ln <- Console.readLn
    _ <- Console.printLn(ln.getOrElse(""))
  } yield ln

  runConsole(justConsoleIo)

}
