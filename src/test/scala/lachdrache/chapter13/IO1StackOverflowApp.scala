package lachdrache.chapter13

import IO1.IO
import IO1.IO._

object IO1StackOverflowApp extends App {

  val app: IO[Unit] =
    forever(PrintLine("I want infinite stack size"))
  app.run

}
