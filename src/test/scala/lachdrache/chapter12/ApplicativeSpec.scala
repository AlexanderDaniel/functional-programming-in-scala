package lachdrache.chapter12

import org.scalatest.FunSpec
import Applicative._

class ApplicativeSpec extends FunSpec {

  describe("optionApplicative") {
    it("combining results: two Some") {
      assertResult(Some("Alice in Dev makes 123 per year")) {
        optionApplicative.map2(Some("Dev"), Some(123)) { (dept, salary) =>
          s"Alice in $dept makes $salary per year"
        }
      }
    }
    it("combining results: one None") {
      assertResult(None) {
        optionApplicative.map2(None, Some(123)) { (dept, salary) =>
          s"Alice in $dept makes $salary per year"
        }
      }
    }
  }
  
  describe("streamApplicative") {
    it("should create a new stream out of two") {
      val ones = streamApplicative.unit(1)
      val twos = streamApplicative.unit(2)
      val combinedStream = streamApplicative.map2(ones, twos) { _+_ }
      assertResult(List(3,3,3)) {
        combinedStream.take(3).toList
      }
    }
  } 
}
