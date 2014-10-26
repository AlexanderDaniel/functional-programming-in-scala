package lachdrache.chapter13

import IO3._

object ConsoleReaderApp extends App {

  val justConsoleIo: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console.")
    ln <- Console.readLn
    _ <- Console.printLn(ln.getOrElse(""))
  } yield ln

  val consoleReader: ConsoleReader[Option[String]] = runConsoleReader(justConsoleIo)

  val maybeString: Option[String] = consoleReader.run("abc")

  println(maybeString)

}
