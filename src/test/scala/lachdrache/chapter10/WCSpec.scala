package lachdrache.chapter10

import org.scalatest.FunSpec
import WC._

class WCSpec extends FunSpec {

  describe("wc") {
    it("2 chars") {
      assertResult(Stub("ab")) {
        wc("ab")
      }
    }
    it("2 words") {
      assertResult(Part("vienna", 0, "scala")) {
        wc("vienna scala")
      }
    }
    it("3 words") {
      assertResult(Part("vienna", 1, "user")) {
        wc("vienna scala user")
      }
    }
    it("4 words") {
      assertResult(Part("vienna", 2, "group")) {
        wc("vienna scala user group")
      }
    }
  }
}

