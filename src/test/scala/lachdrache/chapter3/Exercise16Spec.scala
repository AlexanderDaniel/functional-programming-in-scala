package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

class Exercise16Spec extends Specification {

  "inc" should {
    {
      inc(Nil) === Nil
    }.eg
    
    {
      inc(List(0,1,2)) === List(1,2,3)
    }.eg
  }
}