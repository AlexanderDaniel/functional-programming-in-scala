package lachdrache.chapter7

import org.scalatest.{BeforeAndAfterAll, FunSpec}
import lachdrache.chapter7.Par._
import java.util.concurrent.{TimeUnit, Future, Executors}

class ParSpec extends FunSpec with BeforeAndAfterAll {

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
      val par: Par[Int] = Par.unit(3)
      val future: Future[Int] = Par.run(es)(par)
      assertResult(3) {
        future.get()
      }
    }
  }

  /**
   * Exercise 4
   * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/4.answer.scala answer]]
   */
  describe("asyncF") {
    it("should convert any function to one that evaluates its result asynchronously") {
      val inc: (Int) => Int = (a: Int) => a + 1
      val asyncInc: (Int) => Par[Int] = Par.asyncF(inc)
      val par: Par[Int] = asyncInc(3)
      val future: Future[Int] = Par.run(es)(par)
      assertResult(4) {
        future.get()
      }
    }
  }

  describe("parallel sum") {
    it("should sum the ints") {
      val par: Par[Int] = Par.sum(IndexedSeq(1,2,3,4))
      val future: Future[Int] = Par.run(es)(par)
      assertResult(10) {
        future.get()
      }
    }
  }

  describe("map2 and fork") {
    it("should run thunks in parallel") {
      def one: String = {
        TimeUnit.SECONDS.sleep(1)
        "slept for 1 second"
      }
      def two: String = {
        TimeUnit.SECONDS.sleep(2)
        "slept for 2 seconds"
      }
      val (_, millis) = timeIt {
        val par: Par[String] = map2(fork(unit(one)), fork(unit(two)))((s1, s2) => s"$s1/$s2")
        val future: Future[String] = Par.run(es)(par)
        assertResult("slept for 1 second/slept for 2 seconds") {
          future.get()
        }
      }
      println(s"Took $millis ms")
      assert(millis < 3*1000*1000)
    }
  }

  private def timeIt[A](thunk: => A): (A, Long) = {
    val startMillis = System.currentTimeMillis()
    var endMillis = 0L
    val result = try {
      thunk
    } finally {
      endMillis = System.currentTimeMillis()
    }
    (result, endMillis-startMillis)
  }

  private val es = Executors.newCachedThreadPool()

  override def afterAll() {
    es.shutdown()
    es.awaitTermination(5, TimeUnit.SECONDS)
  }
}
