package lachdrache.chapter5

import org.specs2.mutable.Specification

class StreamSuite extends Specification {

  "stream" should {
    {
      Empty === Empty
    }.eg

    "functions have an Any-line equals implementation" in {
      val f = () => Empty
      val g = () => Empty
      f !== g
    }

    "equals on Cons evals to false because equals on functions evals to false" in {
      val e = Cons(() => 1, () => Empty)
      val a = Cons(() => 1, () => Empty)
      e !== a
    }

    // I.e. we have a case class Cons which provides an implementation
    // for equals. But if the constructor parameters don't provide an
    // implementation of equals the case class does not help of course.

    // But one might argue that two functions with the same implementation
    // should be same. What do you think?

    // Of course one could only say that if the functions do not access
    // any variables outside of their scope.

    // Why is Scala unable to provide this functionality?
    // Would it be sufficient just to compare the bytecode or abstract syntax tree?

    // But that brings us to meta-programming where a function has to
    // know it's own code. I.e. it will bring an overhead to the generated code
    // to include such information.

    // Probably this is related to the trade-off of Java and Scala to not
    // include the type information of generics for the runtime.
  }
}