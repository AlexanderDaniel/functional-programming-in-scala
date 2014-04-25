package lachdrache.chapter6.exercise11

import lachdrache.chapter6.State
import lachdrache.chapter6.State._

object CandyDispenser {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, (Int, Int)](s => ((s.coins, s.candies), s))) {
      (z, in) => update(z)(in match {
        case Coin => m => if (m.candies>0 && m.locked) m.copy(locked=false, coins=m.coins+1) else m
        case Turn => m => if (!m.locked) m.copy(locked=true, candies = m.candies-1) else m
        case _ => identity
      })
    }

  def update(z: State[Machine, (Int, Int)])(f: Machine => Machine): State[Machine, (Int, Int)] =
    z.flatMap(_ => modify(f)).flatMap(_ => get).map(m => (m.coins, m.candies))

}
