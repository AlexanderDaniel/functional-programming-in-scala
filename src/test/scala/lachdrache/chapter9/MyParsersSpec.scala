package lachdrache.chapter9

import org.scalatest.FunSpec
import MyParsers._

class MyParsersSpec extends FunSpec {

  describe("string parser") {
    it("should succeed if the input is the same") {
      assertResult(Right("lachdrache")) {
        MyParsers.run(string("lachdrache"))("lachdrache")
      }
    }
    it("should fail if the input does not match") {
      assert {
        MyParsers.run(string("lachdrache"))("blah blah").isLeft
      }
    }
    it("should succeed if expected string is of length 0") {
      assertResult(Right("")) {
        MyParsers.run(string(""))("something")
      }
    }
  }

  describe("or") {
    val parser = "true" | "false"

    it("should accept the first value") {
      assertResult(Right("true")) {
        MyParsers.run(parser)("true")
      }
    }
    it("should accept the second value") {
      assertResult(Right("false")) {
        MyParsers.run(parser)("false")
      }
    }
  }

  describe("map") {
    val parser = string("true") map { s => s.toBoolean }

    it("should return the value of map") {
      assertResult(Right(true)) {
        MyParsers.run(parser)("true")
      }
    }
  }

// TODO
//  describe("json parser") {
//    it("should parse boolean values") {
//      assertResult(Right(true)) {
//        MyParsers.run(MyJSON.jsonParser(MyParsers))("true")
//      }
//    }
//  }
}