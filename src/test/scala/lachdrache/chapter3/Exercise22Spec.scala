package lachdrache.chapter3

import org.specs2.mutable.Specification

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/22.answer.scala answer]]
 */
class Exercise22Spec extends Specification {

  def zipAddInts(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, zipAddInts(t1, t2))
  }

  "zipAddInts" should {
    {
      zipAddInts(Nil, Nil) === Nil
    }.eg

    {
      zipAddInts(List(1,2,3), List(4,5,6)) === List(5,7,9)
    }.eg

    {
      zipAddInts(List(1,2,3), List(4)) === List(5)
    }.eg

  }
}