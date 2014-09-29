package lachdrache.chapter13

import lachdrache.chapter13.IO2a.IO
import lachdrache.chapter13.IO2a.IO._

object IO2aForever extends App {
  val p: IO[Unit] = forever(printLine("Still going..."))

  run(p)
}
