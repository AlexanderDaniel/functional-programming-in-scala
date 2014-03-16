package lachdrache.chapter4

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/1.answer.scala answer]] */
class Exercise1Spec extends Specification {

  "map" should {
    {
      None.map(identity) === None
    }.eg

    {
      Some(10).map(identity) === Some(10)
    }.eg

    {
      Some(3).map(_+10) === Some(13)
    }.eg
  }

  "getOrElse" should {
    {
      None.getOrElse("default") === "default"
    }.eg

    {
      Some("value").getOrElse("default") === "value"
    }.eg

    "parameter is not evaluated for Some" in {
      Some(13).getOrElse(throw new RuntimeException) === 13
    }
  }

  "flatMap" should {
    {
      None.flatMap(_ => Some(13)) === None
    }.eg

    {
      Some(13).flatMap(v => Some(13+7)) === Some(20)
    }.eg

    {
      Some(13).flatMap(_ => None) === None
    }.eg
  }

  "orElse" should {

    {
      None.orElse(Some(13)) === Some(13)
    }.eg

    {
      None.orElse(None) === None
    }.eg

    {
      Some(13).orElse(None) === Some(13)
    }.eg

    {
      Some(13).orElse(Some(-1)) === Some(13)
    }.eg

  }

  "filter" should {
    {
      None.filter(_ => true) === None
    }.eg

    {
      Some(13).filter(_%2==0) === None
    }.eg

    {
      Some(14).filter(_%2==0) === Some(14)
    }.eg
  }
}