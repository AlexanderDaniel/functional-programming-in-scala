package lachdrache.chapter13

import IO1.IO
import IO1.IO._
import scala.language.postfixOps

object FactorialREPL extends App {

  val helpString = """
                     | The Amazing Factorial REPL, v2.0
                     | q - quit
                     | <number> - compute the factorial of the given number
                     | <anything else> - bomb with horrible error
                   """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream)(i => skip(acc.modify(_ * i)))
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpString) },
    doWhile(ReadLine) { line =>
      when (line != "q") { for {
        n <- factorial(line.toInt)
        _ <- PrintLine(s"factorial: $n")
      } yield () }
    }
  )

  factorialREPL.run
}
