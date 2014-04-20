package lachdrache.chapter6

import org.specs2.mutable.Specification
import lachdrache.chapter6.State._

class Chapter6Spec extends Specification {

  "using state in for-comprehension" should {
    "inc" in {
      val state: State[Int, Int] = for {
        x <- inc
        y <- inc
        z <- inc
      } yield x + y + z
      state.run(1) === (6,4)
    }

    "incs" in {
      val state = for {
        xs <- incs(3)
        y <- inc
        z <- inc
      } yield xs :+ y :+ z
      state.run(1) === (List(1,2,3,4,5), 6)
    }
  }

  val inc = State[Int, Int](s => (s, s+1))

  def incs(n: Int): State[Int, List[Int]] =
    sequence(List.fill(n)(inc))

}