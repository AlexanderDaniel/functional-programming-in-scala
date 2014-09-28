package lachdrache.chapter13

import IO._

object StackOverflowApp extends App {

  val app: IO[Unit] =
    forever(PrintLine("I want infinite stack size"))
  app.run

}
