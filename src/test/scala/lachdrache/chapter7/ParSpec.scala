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

  /** It would not terminate with a single thread executor because one thread blocks
    * in `map2` when calling `get` */
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
      println(s"map2 and fork took $millis ms")
      assert(millis < 3*1000*1000)
    }
  }

  assertSortPar("sortPar via map2", Par.sortParViaMap2)
  assertSortPar("sortPar via map", Par.sortParViaMap)

  assertParMap("parMap", Par.parMap)
  assertParMap("parMap via foldRight and map2", Par.parMapViaFoldRightAndMap2)
  assertParMap("parMap using asyncF", Par.parMapUsingAsyncF)

  describe("sequence") {
    it("converts a list of Par to a Par of a list") {
      val listOfPar: List[Par[Int]] = List(1,2,3,4,5,6,7,8,9) map asyncF(_+10)
      val par: Par[List[Int]] = sequence(listOfPar)
      val future: Future[List[Int]] = Par.run(es)(par)
      assertResult(List(11,12,13,14,15,16,17,18,19)) {
        future.get()
      }
    }
  }

  assertParMap("parMap using sequence", Par.parMapUsingSequence)

  private def assertParMap(name: String, parMap: List[Int] => (Int => Int) => Par[List[Int]]) {
    describe(name) {
      it("applies f to each element of the list") {
        val par: Par[List[Int]] = parMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(_ + 10)
        val future: Future[List[Int]] = Par.run(es)(par)
        assertResult(List(11, 12, 13, 14, 15, 16, 17, 18, 19)) {
          future.get()
        }
      }
      it("should run all operations in parallel") {
        val (_, millis) = timeIt {
          val par: Par[List[Int]] = parMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(a => {
            TimeUnit.SECONDS.sleep(1)
            a + 10
          })
          val future: Future[List[Int]] = Par.run(es)(par)
          assertResult(List(11, 12, 13, 14, 15, 16, 17, 18, 19)) {
            future.get()
          }
        }
        println(s"$name took $millis ms")
        assert(millis < 2 * 1000 * 1000)
      }
    }
  }

  private def assertSortPar(name: String, sortPar: Par[List[Int]] => Par[List[Int]]) {
    describe(name) {
      it("should sort the list in the par") {
        val par: Par[List[Int]] = sortPar(unit(List(4, 1, 3, 2)))
        val future: Future[List[Int]] = Par.run(es)(par)
        assertResult(List(1, 2, 3, 4)) {
          future.get()
        }
      }
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
