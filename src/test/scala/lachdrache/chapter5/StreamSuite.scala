package lachdrache.chapter5

import org.specs2.mutable.Specification
import Stream._

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

  "cons" should {
    {
      var cnt=0
      cons({cnt+=1; 1}, Empty)
      cnt === 0
    }.eg
  }

  "apply" should {
    {
      Stream() === Empty
    }.eg

    /**
     * I tried to define `def nonStrict[A](as: (=>A)*): Stream[A]`
     * but the compiler says that `no by=name parameter type allowed here`
     */
    "uses by-value parameters (strict)" in {
      var cnt=0
      Stream({cnt+=1; 1})
      cnt === 1
    }
  }

  "exercise 1" should {
    (Stream.empty.toList === List()).eg
    (Stream(1).toList === List(1)).eg
    (Stream(1,2,3).toList === List(1,2,3)).eg
  }

  "exercise 1 tailrec" should {
    (Stream.empty.toListTailrec === List()).eg
    (Stream(1).toListTailrec === List(1)).eg
    (Stream(1,2,3).toListTailrec === List(1,2,3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/1.answer.scala answer]] */
  "exercise 1 ListBuffer" should {
    (Stream.empty.toListWithBuffer === List()).eg
    (Stream(1).toListWithBuffer === List(1)).eg
    (Stream(1,2,3).toListWithBuffer === List(1,2,3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/2.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/2.answer.scala answer]]
    */
  "exercise 2: take(n)" should {
    (Stream.empty.take(0).toList === List()).eg
    (Stream.empty.take(1) should throwA[NoSuchElementException]).eg
    (Stream(1,2,3).take(1).toList === List(1)).eg
    (Stream(1,2,3).take(2).toList === List(1,2)).eg
    (Stream(1,2,3).take(3).toList === List(1,2,3)).eg
  }
  "exercise 2: drop(n)" should {
    (Stream.empty.drop(0).toList === List()).eg
    (Stream.empty.drop(1) should throwA[NoSuchElementException]).eg
    (Stream(1,2,3).drop(1).toList === List(2,3)).eg
    (Stream(1,2,3).drop(2).toList === List(3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/3.answer.scala answer]] */
  "exercise 3: takeWhile" should {
    (Stream(1,2,3).takeWhile(_ => true).toList === List(1,2,3)).eg
    (Stream(1,2,3).takeWhile(x => x<3).toList === List(1,2)).eg
    (Stream(1,2,3).takeWhile(x => x<2).toList === List(1)).eg
    (Stream(1,2,3).takeWhile(x => x<1).toList === List()).eg
  }

  "exists" should {
    (Stream(1,2,3).exists(n => n%2==0) === true).eg
    (Stream(1,2,3).exists(n => n>3) === false).eg

    "infinite stream of ones" in {
      lazy val stream: Stream[Int] = cons(1, stream)
      stream.exists(n => n==1) === true
    }

    "stream of natural numbers" in {
      naturalNumbers.exists(_==13) === true
    }
  }

  "fibonacci numbers" should {
    {
      fibs.take(3).toList === List(1,1,2)
      fibs.take(7).toList === List(1,1,2,3,5,8,13)
    }.eg
  }

  "foldRight" should {
    {
      Stream(1,2,3).foldRight(0)(_+_) === 6
    }.eg

    {
      Stream(1,2,3).foldRight("0")((a, z) => s"f($a, => $z)") === "f(1, => f(2, => f(3, => 0)))"
    }.eg
  }

  "exists via foldRight" should {
    {
      naturalNumbers.exists(_==42) === true
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/4.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/4.answer.scala answer]]
    */
  "exercise 4: forall" should {
    {
      Stream().forall(_ => false) === true
    }.eg

    {
      naturalNumbers.forall(_ => false) === false
    }.eg

    {
      naturalNumbers.take(9).forall(_<10) === true
    }.eg

    {
      naturalNumbers.take(10).forall(_<10) === false
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/5.answer.scala answer]] */
  "exercise 5: takeWhile via foldRight" should {
    (Stream(1,2,3).takeWhileViaFoldRight(_ => true).toList === List(1,2,3)).eg
    (Stream(1,2,3).takeWhileViaFoldRight(x => x<3).toList === List(1,2)).eg
    (Stream(1,2,3).takeWhileViaFoldRight(x => x<2).toList === List(1)).eg
    (Stream(1,2,3).takeWhileViaFoldRight(x => x<1).toList === List()).eg
  }

  "exercise 6: headOption via foldRight" should {
    {
      Stream().headOption === None
    }.eg

    {
      Stream(1).headOption === Some(1)
    }.eg

    {
      cons(1, cons({sys.error(""); 2}, Empty)).headOption === Some(1)
    }.eg
  }

  "exercise 7: map" should {
    {
      Stream().map(identity) === Stream()
    }.eg

    {
      Stream(0,1,2).map(_+1).toList === List(1,2,3)
    }.eg

    {
      naturalNumbers.map(_-1).take(5).toList === List(0,1,2,3,4)
    }.eg
  }

  "exercise 7: filter" should {
    {
      Stream(1,2,3,4,5).filter(_%2==0).toList === List(2,4)
    }.eg

    {
      fibs.filter(_<=13).take(7).toList === List(1,1,2,3,5,8,13)
    }.eg
  }

  "exercise 7: append" should {
    {
      Stream(1,2).append(Stream(3,4)).toList === List(1,2,3,4)
    }.eg

    {
      Stream(0).append(fibs).take(5).toList === List(0,1,1,2,3)
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/6.answer.scala answer]]
    * of exercise 6 applies to exercise 7
    */
  "exercise 7: flatMap" should {
    {
      Stream(2,3).flatMap(n => naturalNumbers.take(n)).toList === List(1,2,1,2,3)
    }.eg

    {
      naturalNumbers.take(5).flatMap(n => Stream(n.toString)).toList === List("1", "2", "3", "4", "5")
    }.eg
  }

  "find" should {
    {
      naturalNumbers.find(_ == 13) === Some(13)
    }.eg

    {
      naturalNumbers.take(12).find(_==13) === None
    }.eg
  }

  "ones" should {
    {
      ones.take(3).toList === List(1,1,1)
    }.eg
  }

}