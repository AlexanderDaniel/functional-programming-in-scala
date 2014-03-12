package lachdrache.chapter3

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/24.answer.scala answer]] */
class Exercise24Spec extends Specification {

  @annotation.tailrec
  private def startsWith[A](l: List[A], sub: List[A]):Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => startsWith(t1,t2)
    case _ => false
  }

  @annotation.tailrec
  private def hasSubsequence[A](l: List[A], sub: List[A]):Boolean = l match {
    case Nil => false
    case Cons(_, t) => if (startsWith(l, sub)) true else hasSubsequence(t, sub)
  }

  "hasSubsequence" should {
    {
      hasSubsequence(List(1,2,3), List(1)) === true
    }.eg

    {
      hasSubsequence(List(1), List(4)) === false
    }.eg

    {
      hasSubsequence(List(1,2,3,4), List(1,2)) === true
    }.eg

    {
      hasSubsequence(List(1,2,3,4), List(2,3)) === true
    }.eg

    {
      hasSubsequence(List(1,2,3,4), List(4)) === true
    }.eg
  }

  "startsWith" should {
    {
      startsWith(List(1), Nil) === true
    }.eg

    {
      startsWith(List(1,2), List(1)) === true
    }.eg

    {
      startsWith(List(1,2), List(2)) === false
    }.eg

    {
      startsWith(List(1,2,3,4,5), List(1,2,3,4,5)) === true
    }.eg

    {
      startsWith(List(1,2), List(1,2,3)) === false
    }.eg

    {
      startsWith(List(1,2,4), List(1,2,3)) === false
    }.eg

  }
}