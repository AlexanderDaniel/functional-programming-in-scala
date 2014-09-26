package lachdrache.chapter13

import IO._

object Converter extends App {

  def fahrenheitToCelsius(f: Double) =
    (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  // interpreter that will actually execute those effects
  converter.run
}
