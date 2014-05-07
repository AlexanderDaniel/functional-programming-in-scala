package lachdrache.chapter5

import org.specs2.mutable.Specification
import Stream._

class InitialExampleSpec extends Specification {

  "just a single pass" should {
    {
      val stream = cons(n(1), cons(n(2), Stream.empty)).map(_ + 10).filter(_ % 2 == 0)
      stream.toList === List(12)
    }.eg
  }

  def n(i: Int) = {
    println(i)
    i
  }
}