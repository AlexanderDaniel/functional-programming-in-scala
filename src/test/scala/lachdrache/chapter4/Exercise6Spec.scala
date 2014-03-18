package lachdrache.chapter4

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/6.answer.scala answer]] */
class Exercise6Spec extends Specification {

  "map" should {
    {
      Left("error").map(_ => sys.error("never called")) === Left("error")
    }.eg

    {
      Right("success").map(_.toUpperCase) === Right("SUCCESS")
    }.eg
  }

  "flatMap" should {
    {
      Left("error").flatMap(_ => sys.error("never called")) === Left("error")
    }.eg

    {
      Right("success").flatMap(s => Right(s.toUpperCase)) === Right("SUCCESS")
    }.eg

    {
      Right("success").flatMap(_ => Left("error")) === Left("error")
    }.eg
  }

  "orElse" should {
    {
      Left("err1").orElse(Left("err2")) === Left("err2")
    }.eg

    {
      Left("err").orElse(Right("yes")) === Right("yes")
    }.eg

    {
      Right("first").orElse(Right("second")) === Right("first")
    }.eg

    {
      Right("first").orElse(Left("err")) === Right("first")
    }.eg
  }

  "map2" should {
    {
      Right(10).map2(Right(3))(_+_) === Right(13)
    }.eg

    {
      Right(1).map2(Left(2))(_+_) === Left(2)
    }.eg

    {
      Left(1).map2(Left(3))((_,_) => sys.error("err")) === Left(1)
    }.eg

    {
      Left(1).map2(Right(2))((_,_) => sys.error("err")) === Left(1)
    }.eg

  }
}