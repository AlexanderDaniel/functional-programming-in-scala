package lachdrache.chapter7

import org.scalatest.FunSpec
import lachdrache.chapter7.Par._
import java.util.concurrent.{TimeUnit, Future, Executors, ExecutorService}

class ParSpec extends FunSpec {

  describe("non-parallel divide and conquer sum") {
    it("should sum the ints") {
      assertResult(10) {
        sumDivideAndConquer(IndexedSeq(1,2,3,4))
      }
    }
  }

  describe("unit and run") {
    /** Actually it does not really need the executor service because UnitFuture does not need it */
    it("should return the initial value") {
      withExecutor { es =>
        val par: Par[Int] = Par.unit(3)
        val future: Future[Int] = Par.run(es)(par)
        assertResult(3) {
          future.get()
        }
      }
    }
  }

  private def withExecutor[T](thunk: ExecutorService => T): T = {
    val es = Executors.newSingleThreadExecutor()
    val result = thunk(es)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
    result
  }
}
