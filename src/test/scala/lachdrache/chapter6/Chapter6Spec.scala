package lachdrache.chapter6

import org.specs2.mutable.Specification
import lachdrache.chapter6.State._

class Chapter6Spec extends Specification {

  "using state in for-comprehension" should {
    val inc = State[Int, Int](s => (s, s+1))

    def incs(n: Int): State[Int, List[Int]] =
      sequence(List.fill(n)(inc))

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

  "get" should {
    val state = State[Boolean, Int](s => (if (s) 1 else -1, s))

    {
      get.run(true) === (true, true)
    }.eg

    {
      get.run(false) === (false, false)
    }.eg

    {
      state.flatMap(_ => get).run(true) === (true, true)
    }.eg

    {
      state.flatMap(_ => get).run(false) === (false, false)
    }.eg
  }

  "set" should {
    val state = State[Boolean, Int](s => (if (s) 1 else -1, s))
    val negateState = state.flatMap(_ => get).flatMap(s => set(!s))

    {
      state.flatMap(_ => set(false)).run(true) === ((), false)
    }.eg

    "negate state" in {
      negateState.run(true) === ((), false)
      negateState.run(false) === ((), true)
    }

    {
      state.flatMap(_ => set(false)).flatMap(_ => get).run(true) === (false, false)
    }.eg

    {
      negateState.flatMap(_ => get).run(true) === (false, false)
    }.eg
  }

  "modify" should {
    val state = State[Boolean, Int](s => (if (s) 1 else -1, s))

    {
      state.run(true) === (1, true)
    }.eg

    {
      state.run(false) === (-1, false)
    }.eg

    {
      state.flatMap(s => modify(_ => false)).run(true) === ((), false)
    }.eg

    {
      state.flatMap(s => modify(_ => false)).run(false) === ((), false)
    }.eg

    {
      state.flatMap(s => modify(_ => true)).run(false) === ((), true)
    }.eg

    {
      state.flatMap(s => modify(s => !s)).run(false) === ((), true)
    }.eg

    {
      state.flatMap(s => modify(s => !s)).run(true) === ((), false)
    }.eg
  }

}