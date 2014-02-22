package lachdrache.chapter2

import org.specs2.mutable.Specification

/** Scala compiler complains when doing `inc andThen square`:
  * {{{
  * missing arguments for method inc;
  * follow this method with _' if you want to treat it as a partially applied function
  * }}}
  *
  * Partially applied functions are very well explained in
  * [[http://www.artima.com/shop/programming_in_scala Programming in Scala, Second Edition]]
  */
class FunctionsAsFirstClassValuesSpec extends Specification {

  "a function can be used as first-class value" should {
    def inc(x: Int) = x + 1
    def square(x: Int) = 2 * x

    "when using def" in {
      (inc _ andThen square)(1) === 4
    }

    "a partially applied function can be stored in a val" in {
      val increment = inc _
      increment(12) === 13
    }
  }

}