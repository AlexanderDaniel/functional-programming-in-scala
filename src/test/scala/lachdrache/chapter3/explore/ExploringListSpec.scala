package lachdrache.chapter3.explore

import org.specs2.mutable.Specification

/** Exploring the functions available in [[scala.collection.immutable.List]] and it's compansion object */
class ExploringListSpec extends Specification {

  "iterate" should {
    {
      List.iterate(1, 5)(x => x+1) === List(1,2,3,4,5)
    }.eg

    {
      List.iterate(1, 5)(x => x+2) === List(1,3,5,7,9)
    }.eg
  }

  "range" should {
    {
      List.range(1, 6) === List(1,2,3,4,5)
    }.eg

    {
      List.range(0, 20, 5) === List(0, 5, 10, 15)
    }.eg
  }

  "tabulate" should {
    {
      List.tabulate(5)(x => x*x) === List(0,1,4,9,16)
    }.eg

    {
      List.tabulate(3, 2)((x, y) => x+y) === List(List(0,1), List(1,2), List(2,3))
    }.eg

    {
      List.tabulate(2, 3)((x, y) => s"$x$y") === List(List("00", "01", "02"), List("10", "11", "12"))
    }.eg
  }

  "concat" should {
    {
      List.concat(List(1,2), List(3), List(4,5), List(6)) === List(1,2,3,4,5,6)
    }.eg
  }

  "fill" should {
    {
      List.fill(2,3)('a) === List(List('a,'a,'a),List('a,'a,'a))
    }.eg
  }
}