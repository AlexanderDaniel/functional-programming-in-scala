package lachdrache.chapter6.exercise11

import lachdrache.chapter6.State

object CandyDispenser {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, (Int, Int)](s => ((s.coins, s.candies), s))) {
      (z, in) => z
    }

}
