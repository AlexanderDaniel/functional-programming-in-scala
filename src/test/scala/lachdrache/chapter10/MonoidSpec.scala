package lachdrache.chapter10

import org.scalatest.FunSpec
import Monoid._

class MonoidSpec extends FunSpec {

  describe("stringMonoid") {
    it("zero") {
      assert(stringMonoid.zero === "")
    }
    it("op") {
      assertResult("alex") {
        stringMonoid.op("al", "ex")
      }
    }
  }
}