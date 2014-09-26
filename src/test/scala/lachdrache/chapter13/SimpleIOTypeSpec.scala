package lachdrache.chapter13

import org.scalatest.FunSpec
import SimpleIOType._

class SimpleIOTypeSpec extends FunSpec {

  describe("contest") {
    it("should yield an IO type which can be run to print the result") {
      val io: SimpleIOType.IO = contest(Player("MrA", score=3), Player("MrB  ", score=5))
      io.run()
    }
  }
}
