package lachdrache.chapter7

import org.scalatest.FunSpec
import lachdrache.chapter7.Par._

class ParSpec extends FunSpec {

  describe("non-parallel divide and conquer sum") {
    it("should sum the ints") {
      assertResult(10) {
        sumDivideAndConquer(IndexedSeq(1,2,3,4))
      }
    }
  }
}
