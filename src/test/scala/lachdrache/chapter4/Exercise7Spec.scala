package lachdrache.chapter4

import org.specs2.mutable.Specification
import lachdrache.chapter4.Either._

class Exercise7Spec extends Specification {

  "sequence" should {
    {
      sequence(Nil) === Right(Nil)
    }.eg

    {
      sequence(List(Right(1))) === Right(List(1))
    }.eg

    {
      sequence(List(Right(1), Right(2), Right(3))) === Right(List(1,2,3))
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