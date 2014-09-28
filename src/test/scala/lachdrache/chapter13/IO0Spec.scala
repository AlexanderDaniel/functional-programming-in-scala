package lachdrache.chapter13

import org.scalatest.FunSpec
import IO0._

class IO0Spec extends FunSpec {

  describe("contest") {
    it("should yield an IO type which can be run to print the result") {
      val io: IO0.IO = contest(Player("MrA", score=3), Player("MrB  ", score=5))
      io.run()
    }
  }
}
