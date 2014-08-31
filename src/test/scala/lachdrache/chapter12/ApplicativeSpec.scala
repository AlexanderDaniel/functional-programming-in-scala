package lachdrache.chapter12

import java.util.Date

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
      val combinedStream = streamApplicative.map2(ones, twos) {
        _ + _
      }
      assertResult(List(3, 3, 3)) {
        combinedStream.take(3).toList
      }
    }
  }

  describe("exercise 4: the meaning of streamApplicative.sequence") {
    it("example 1") {
      assertResult(List(List(1), List(1), List(1))) {
        streamApplicative.sequence(List(Stream.continually(1))).take(3).toList
      }
    }
    it("example 2") {
      assertResult(List(List(1, 4), List(2, 5), List(3, 6))) {
        streamApplicative.sequence(List(Stream(1, 2, 3), Stream(4, 5, 6))).take(3).toList
      }
    }
    it("example 3") {
      assertResult(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) {
        streamApplicative.sequence(List(
          Stream.from(1, step = 3),
          Stream.from(2, step = 3),
          Stream.from(3, step = 3)
        )).take(3).toList
      }
    }
  }

  describe("validation applicative") {
    def validName(name: String): Validation[String, String] =
      if (name != "") Success(name)
      else Failure("Name cannot be empty")

    def validBirthDate(birthDate: String): Validation[String, Date] =
      try {
        import java.text._
        Validation.success[String, Date](new SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
      } catch {
        case _: Exception => Validation.failure[String, Date]("Birth date must be in the form yyyy-MM-dd")
      }

    def validPhone(phoneNumber: String): Validation[String, String] =
      if (phoneNumber.matches("[0-9]{10}"))
        Success(phoneNumber)
      else Failure("Phone number must be 10 digits")

    it("should accumulate errors") {
      assertResult(Failure("Name cannot be empty", Vector("Birth date must be in the form yyyy-MM-dd", "Phone number must be 10 digits"))) {
        validationApplicative[String].map3(
          validName(""),
          validBirthDate(""),
          validPhone("")
        )((_, _, _))
      }
    }
  }

  describe("map2") {
    it("first a than b") {
      assertResult(Failure("a", Vector("b"))) {
        validationApplicative.map2(Failure("a"), Failure("b"))((_, _))
      }
    }
  }

  describe("sequenceMap") {
    it("converts a map of applicative values to an applicative of map") {
      val input = Map(
        1 -> validationApplicative.unit("one"),
        2 -> validationApplicative.unit("two"),
        3 -> validationApplicative.unit("three")
      )
      assertResult(validationApplicative.unit(Map(1 -> "one", 2 -> "two", 3 -> "three"))) {
        validationApplicative.sequenceMap(input)
      }
    }
  }
}
