package lachdrache.chapter8

import org.scalatest.FunSpec
import lachdrache.chapter6.RNG

class GenSpec extends FunSpec {

  describe("Gen.unit") {
    it("should store a unit value") {
      val gen: Gen[String] = Gen.unit("FPF")
      assertResult(("FPF", constRng13)) {
        gen.sample.run(constRng13)
      }
    }
  }

  describe("Gen.boolean") {
    it("should return true for negative number") {
      val constRng: ConstRng = ConstRng(-1)
      val gen: Gen[Boolean] = Gen.boolean
      assertResult((true, constRng)) {
        gen.sample.run(constRng)
      }
    }
    it("should return false for positive number") {
      val constRng: ConstRng = ConstRng(3)
      val gen: Gen[Boolean] = Gen.boolean
      assertResult((false, constRng)) {
        gen.sample.run(constRng)
      }
    }
  }

  describe("union") {
    it("should choose one of the generators with equal likelihood") {
      val gen: Gen[Int] = Gen.union(Gen.unit(1), Gen.unit(2))

      assertResult((1, ConstRng(-1))) {
        gen.sample.run(ConstRng(-1))
      }
    }
  }

  private case class NaturalNumbers(current: Int) extends RNG {
    override def nextInt: (Int, RNG) = (current, NaturalNumbers(current+1))
  }
  private case class ConstRng(v: Int) extends RNG {
    override def nextInt: (Int, RNG) = (v, this)
  }
  private val constRng13: ConstRng = ConstRng(13)

}