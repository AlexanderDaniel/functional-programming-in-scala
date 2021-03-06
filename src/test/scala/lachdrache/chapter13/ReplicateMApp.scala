package lachdrache.chapter13

import IO1.IO
import IO1.IO._

object ReplicateMApp extends App {

  val threeLines: IO[List[String]] = replicateM(3, ReadLine)

  threeLines.flatMap(l => PrintLine(l.toString)).run
}
