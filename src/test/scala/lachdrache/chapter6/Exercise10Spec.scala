package lachdrache.chapter6

import org.specs2.mutable.Specification
import lachdrache.chapter6.State._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/12.answer.scala answer]] */
class Exercise10Spec extends Specification {

  "unit" should {
    {
      unit(13).run(None) === (13,None)
    }.eg

    {
      unit(16).run(None) === (16,None)
    }.eg
  }

  "map" should {
    {
      unit(2).map(a => a.toString).run(None) === ("2", None)
    }.eg

    "inc" in {
      inc.map(a => a.toString).run(1) === ("1", 2)
    }
    "chaining" in {
      inc.map(_*10).map(_.toString).run(1) === ("10", 2)
    }
  }

  "map2" should {
    {
      unit[String, Int](1).map2(unit(2))((a,b) => s"$a,$b").run("n/a") === ("1,2", "n/a")
    }.eg

    "inc" in {
      inc.map2(inc)((a,b) => s"$a•$b").run(1) === ("1•2", 3)
    }
  }

  "flatMap" should {
    {
      unit[String, Int](1).flatMap(a => unit(a*10)).run("n/a") === (10, "n/a")
    }.eg
  }

  "sequence" should {
    {
      sequence[String,Int](List(unit(1), unit(2), unit(3))).run("n/a") === (List(1,2,3), "n/a")
    }.eg

    "inc" in {
      sequence(List(inc, inc, inc)).run(1) === (List(1,2,3), 4)
    }
  }

  val inc = State[Int, Int](s => (s, s+1))

}