package lachdrache.chapter13

import IO3._

object ConsoleStateApp extends App {

  val justConsoleIo: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console.")
    ln <- Console.readLn
    _ <- Console.printLn(ln.getOrElse(""))
  } yield ln

  val consoleState: ConsoleState[Option[String]] = runConsoleState(justConsoleIo)

  val result: (Option[String], Buffers) = consoleState.run(Buffers(List("VSUG"), Vector()))

  println(result)

}
