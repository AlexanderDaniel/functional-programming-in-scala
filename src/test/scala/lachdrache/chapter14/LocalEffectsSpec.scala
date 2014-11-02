package lachdrache.chapter14

import org.scalatest.FunSpec
import QuicksortUsingST.quicksort

import scala.util.Random

class LocalEffectsSpec extends FunSpec {

  describe("quicksort") {
    it("should sort the list") {
      val list = (1 to 100).toList
      val shuffled = Random.shuffle(list)
      assertResult(list) {
        quicksort(shuffled)
      }
    }
  }
}
