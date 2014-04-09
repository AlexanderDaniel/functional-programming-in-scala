package lachdrache.chapter5

import org.specs2.mutable.Specification
import Stream._

class StreamSpec extends Specification {

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
      var cnt = 0
      cons({
        cnt += 1; 1
      }, Empty)
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
      var cnt = 0
      Stream({
        cnt += 1; 1
      })
      cnt === 1
    }
  }

  "exercise 1" should {
    (Stream.empty.toList === List()).eg
    (Stream(1).toList === List(1)).eg
    (Stream(1, 2, 3).toList === List(1, 2, 3)).eg
  }

  "exercise 1 tailrec" should {
    (Stream.empty.toListTailrec === List()).eg
    (Stream(1).toListTailrec === List(1)).eg
    (Stream(1, 2, 3).toListTailrec === List(1, 2, 3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/1.answer.scala answer]] */
  "exercise 1 ListBuffer" should {
    (Stream.empty.toListWithBuffer === List()).eg
    (Stream(1).toListWithBuffer === List(1)).eg
    (Stream(1, 2, 3).toListWithBuffer === List(1, 2, 3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/2.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/2.answer.scala answer]]
    */
  "exercise 2: take(n)" should {
    (Stream.empty.take(0).toList === List()).eg
    (Stream.empty.take(1) should throwA[NoSuchElementException]).eg
    (Stream(1, 2, 3).take(1).toList === List(1)).eg
    (Stream(1, 2, 3).take(2).toList === List(1, 2)).eg
    (Stream(1, 2, 3).take(3).toList === List(1, 2, 3)).eg
  }
  "exercise 2: drop(n)" should {
    (Stream.empty.drop(0).toList === List()).eg
    (Stream.empty.drop(1) should throwA[NoSuchElementException]).eg
    (Stream(1, 2, 3).drop(1).toList === List(2, 3)).eg
    (Stream(1, 2, 3).drop(2).toList === List(3)).eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/3.answer.scala answer]] */
  "exercise 3: takeWhile" should {
    (Stream(1, 2, 3).takeWhile(_ => true).toList === List(1, 2, 3)).eg
    (Stream(1, 2, 3).takeWhile(x => x < 3).toList === List(1, 2)).eg
    (Stream(1, 2, 3).takeWhile(x => x < 2).toList === List(1)).eg
    (Stream(1, 2, 3).takeWhile(x => x < 1).toList === List()).eg
  }

  "exists" should {
    (Stream(1, 2, 3).exists(n => n % 2 == 0) === true).eg
    (Stream(1, 2, 3).exists(n => n > 3) === false).eg

    "infinite stream of ones" in {
      lazy val stream: Stream[Int] = cons(1, stream)
      stream.exists(n => n == 1) === true
    }

    "stream of natural numbers" in {
      naturalNumbers.exists(_ == 13) === true
    }
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/9.answer.scala answer]] */
  "exercise 10: fibonacci numbers" should {
    {
      fibs.take(3).toList === List(1, 1, 2)
      fibs.take(7).toList === List(1, 1, 2, 3, 5, 8, 13)
    }.eg
  }

  "foldRight" should {
    {
      Stream(1, 2, 3).foldRight(0)(_ + _) === 6
    }.eg

    {
      Stream(1, 2, 3).foldRight("0")((a, z) => s"f($a, => $z)") === "f(1, => f(2, => f(3, => 0)))"
    }.eg
  }

  "exists via foldRight" should {
    {
      naturalNumbers.exists(_ == 42) === true
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
      naturalNumbers.take(9).forall(_ < 10) === true
    }.eg

    {
      naturalNumbers.take(10).forall(_ < 10) === false
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/5.answer.scala answer]] */
  "exercise 5: takeWhile via foldRight" should {
    (Stream(1, 2, 3).takeWhileViaFoldRight(_ => true).toList === List(1, 2, 3)).eg
    (Stream(1, 2, 3).takeWhileViaFoldRight(x => x < 3).toList === List(1, 2)).eg
    (Stream(1, 2, 3).takeWhileViaFoldRight(x => x < 2).toList === List(1)).eg
    (Stream(1, 2, 3).takeWhileViaFoldRight(x => x < 1).toList === List()).eg
  }

  "exercise 6: headOption via foldRight" should {
    {
      Stream().headOption === None
    }.eg

    {
      Stream(1).headOption === Some(1)
    }.eg

    {
      cons(1, cons({
        sys.error(""); 2
      }, Empty)).headOption === Some(1)
    }.eg
  }

  "exercise 7: map" should {
    {
      Stream().map(identity) === Stream()
    }.eg

    {
      Stream(0, 1, 2).map(_ + 1).toList === List(1, 2, 3)
    }.eg

    {
      naturalNumbers.map(_ - 1).take(5).toList === List(0, 1, 2, 3, 4)
    }.eg
  }

  "exercise 7: filter" should {
    {
      Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList === List(2, 4)
    }.eg

    {
      fibs.filter(_ <= 13).take(7).toList === List(1, 1, 2, 3, 5, 8, 13)
    }.eg
  }

  "exercise 7: append" should {
    {
      Stream(1, 2).append(Stream(3, 4)).toList === List(1, 2, 3, 4)
    }.eg

    {
      Stream(0).append(fibs).take(5).toList === List(0, 1, 1, 2, 3)
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/6.answer.scala answer]]
    * of exercise 6 applies to exercise 7
    */
  "exercise 7: flatMap" should {
    {
      Stream(2, 3).flatMap(n => naturalNumbers.take(n)).toList === List(1, 2, 1, 2, 3)
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
      naturalNumbers.take(12).find(_ == 13) === None
    }.eg
  }

  "ones" should {
    {
      ones.take(3).toList === List(1, 1, 1)
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/7.answer.scala answer]] */
  "exercise 8: constant" should {
    {
      constant("z").take(5).toList === List("z", "z", "z", "z", "z")
    }.eg
  }

  // I already did exercise 9 before it was mentioned :-)
  // https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/8.answer.scala
  // is still based on an old revision of the MEAP :-(

  // Same for exercise 10. Can be found above

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/10.answer.scala answer exercise 11]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/11.answer.scala answer exercise 12]] */
  "exercise 11 & 12: unfold" should {

    "create a stream of 1s" in {
      val streamOfOnes: Stream[Int] = unfold(1)(s => Some((s, s)))
      streamOfOnes.take(5).toList === List(1, 1, 1, 1, 1)
    }

    "create the natural numbers" in {
      val natural = unfold(1)(s => Some((s, s + 1)))
      natural.take(5).toList === List(1, 2, 3, 4, 5)
    }

    "fibonacci numbers" in {
      val fib = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
      fib.take(8).toList === List(0, 1, 1, 2, 3, 5, 8, 13)
    }

    "even numbers" in {
      val even = unfold(0)(s => Some((s, s + 2)))
      even.take(4).toList === List(0, 2, 4, 6)
    }

    "square of natural numbers" in {
      val square = unfold(1)(s => Some((s * s, s + 1)))
      square.take(5).toList === List(1, 4, 9, 16, 25)
    }

    "stream from 5 to 1" in {
      val fiveToZero = unfold(5) {
        case 0 => None
        case s => Some((s, s - 1))
      }
      fiveToZero.toList === List(5, 4, 3, 2, 1)
    }
  }

  "exercise 13: map via unfold" should {
    {
      Stream().mapViaUnfold(identity) === Stream()
    }.eg

    {
      Stream(0, 1, 2).mapViaUnfold(_ + 1).toList === List(1, 2, 3)
    }.eg

    {
      naturalNumbers.mapViaUnfold(_ - 1).take(5).toList === List(0, 1, 2, 3, 4)
    }.eg
  }

  "exercise 13: take via unfold" should {
    (Stream.empty.takeViaUnfold(0).toList === List()).eg
    (Stream(1, 2, 3).takeViaUnfold(1).toList === List(1)).eg
    (Stream(1, 2, 3).takeViaUnfold(2).toList === List(1, 2)).eg
    (Stream(1, 2, 3).takeViaUnfold(3).toList === List(1, 2, 3)).eg
  }
  
  "exercise 13: takeWhile via unfold" should {
    (Stream(1, 2, 3).takeWhileViaUnfold(_ => true).toList === List(1, 2, 3)).eg
    (Stream(1, 2, 3).takeWhileViaUnfold(x => x < 3).toList === List(1, 2)).eg
    (Stream(1, 2, 3).takeWhileViaUnfold(x => x < 2).toList === List(1)).eg
    (Stream(1, 2, 3).takeWhileViaUnfold(x => x < 1).toList === List()).eg
  }

  "exercise 13: zipWith via unfold" should {
    {
      Stream(1,2,3).zipWith(Stream(3,2,1))(_+_).toList === List(4,4,4)
    }.eg

    "streams of different length" in {
      val s1 = naturalNumbers take 5
      val s2 = constant(10)
      s1.zipWith(s2)(_+_).toList === List(11,12,13,14,15)
    }
  }

  "exercise 13: zip" should {
    "zip the elements of two streams" in {
      val s1 = Stream("a", "b", "c")
      val s2 = naturalNumbers
      s1.zip(s2).toList === List(("a", 1), ("b", 2), ("c", 3))
    }
  }

  "exercise 13: zipAll via unfold" should {
    "zip the elements of two streams" in {
      val s1 = Stream("a")
      val s2 = naturalNumbers
      s1.zipAll(s2).take(3).toList === List((Some("a"), Some(1)), (None, Some(2)), (None, Some(3)))
    }
  }

  "exercise 13: zipAll2" should {
    "zip the elements of two streams" in {
      val s1 = Stream("a")
      val s2 = naturalNumbers
      s1.zipAll(s2).take(3).toList === List((Some("a"), Some(1)), (None, Some(2)), (None, Some(3)))
    }
  }

  // answers to exercise 13 at https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/12.answer.scala


  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/13.answer.scala answer]]
    * of the authors is in `startsWith2`
    */
  "exercise 14: startsWith" should {
    {
      Stream(1).startsWith(Stream()) === true
    }.eg

    {
      Stream().startsWith(Stream()) === true
    }.eg

    {
      Stream(1,2,3).startsWith(Stream(1,2)) === true
    }.eg

    {
      Stream(1,2,3).startsWith(Stream(1,2,3)) === true
    }.eg

    {
      Stream(1,2,3).startsWith(Stream(1)) === true
    }.eg

    {
      Stream(1,2,3).startsWith(Stream(2)) === false
    }.eg

    {
      Stream(1,2,3).startsWith(Stream(1,2,3,4)) === false
    }.eg
  }

  "exercise 14: startsWith2" should {
    {
      startsWith2(Stream(1), Stream()) === true
    }.eg

    {
      startsWith2(Stream(), Stream()) === true
    }.eg

    {
      startsWith2(Stream(1,2,3), Stream(1,2)) === true
    }.eg

    {
      startsWith2(Stream(1,2,3), Stream(1,2,3)) === true
    }.eg

    {
      startsWith2(Stream(1,2,3), Stream(1)) === true
    }.eg

    {
      startsWith2(Stream(1,2,3), Stream(2)) === false
    }.eg

    {
      startsWith2(Stream(1,2,3), Stream(1,2,3,4)) === false
    }.eg
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/laziness/14.answer.scala answer]] */
  "exercise 15: tails" should {
    {
      Stream().tails.map(_.toList).toList === List(List())
    }.eg

    {
      Stream(1).tails.map(_.toList).toList === List(List(1), List())
    }.eg

    {
      Stream(1,2).tails.map(_.toList).toList === List(List(1,2), List(2), List())
    }.eg

    {
      Stream(1,2,3).tails.map(_.toList).toList === List(List(1,2,3), List(2,3), List(3), List())
    }.eg
  }

  "hasSubsequence" should {
    {
      Stream(1,2,3).hasSubsequence(Stream(2)) === true
    }.eg

    {
      Stream(1,2,3).hasSubsequence(Stream(1,3)) === false
    }.eg

    {
      Stream(1,2,3).hasSubsequence(Stream(1,2,3)) === true
    }.eg

    {
      Stream(1,2,3).hasSubsequence(Stream()) === true
    }.eg
  }

}