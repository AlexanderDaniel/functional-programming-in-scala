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

  describe("foldMapV") {
    it("should work like foldMap") {
      assertResult("alex") {
        foldMapV(Vector("a", "l", "e", "x"), stringMonoid)(identity)
      }
    }
    it("should work like foldMap for empty seq") {
      assertResult("") {
        foldMapV(Vector(), stringMonoid)(identity)
      }
    }
    it("should work like foldMap for one element") {
      assertResult("ali") {
        foldMapV(Vector("ali"), stringMonoid)(identity)
      }
    }
  }
}