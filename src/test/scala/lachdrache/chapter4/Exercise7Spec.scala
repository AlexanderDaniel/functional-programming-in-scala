package lachdrache.chapter4

import org.specs2.mutable.Specification
import lachdrache.chapter4.Either._

class Exercise7Spec extends Specification {

  assertSequence("sequence via recursion", sequence)
  assertSequence("sequence via traverse", sequenceViaTraverse)

  def assertSequence(s: String, sequence: List[Either[String, Int]] => Either[String, List[Int]]) = {
    s should {
      {
        sequence(Nil) === Right(Nil)
      }.eg

      {
        sequence(List(Right(1))) === Right(List(1))
      }.eg

      {
        sequence(List(Right(1), Right(2), Right(3))) === Right(List(1, 2, 3))
      }.eg

      {
        sequence(List(Left("err1"))) === Left("err1")
      }.eg

      {
        sequence(List(Right(1), Left("err1"))) === Left("err1")
      }.eg

      {
        sequence(List(Right(1), Right(2), Left("err1"))) === Left("err1")
      }.eg

      {
        sequence(List(Right(1), Left("err1"), Right(2))) === Left("err1")
      }.eg
    }
  }

  "traverse" should {
    {
      traverse(Nil)(identity) === Right(Nil)
    }.eg

    {
      traverse(List(1,2,3))(a => Right(a*10)) === Right(List(10,20,30))
    }.eg

    {
      traverse(List(1,2,3))(a => Left("err" + a)) === Left("err1")
    }.eg

    {
      traverse(List(1,2,3))(a => if (a%2==0) Left("odd"+a) else Right(a)) === Left("odd2")
    }.eg
  }
}