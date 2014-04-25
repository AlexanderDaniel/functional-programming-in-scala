package lachdrache.chapter6.exercise11

import org.specs2.mutable.Specification
import lachdrache.chapter6.State._

class CandyDispenserSpec extends Specification {

  "empty list of input" should {
    "not change the state" in {
      val machine = CandyDispenser.simulateMachine(List())
      val initialState = Machine(locked = false, coins = 0, candies = 13)
      machine.run(initialState) === ((0,13), initialState)
    }
  }

  "Inserting a coin into a locked machine will cause it to unlock if there is any candy left" should {
    "coin into locked machine with left candy" in {
      val machine = CandyDispenser.simulateMachine(List(Coin))
      val initialState = Machine(locked = true, coins = 0, candies = 13)
      machine.run(initialState) === ((1,13), Machine(locked = false, coins=1, candies=13))
    }
  }

//  "coin" should {
//    {
//      CandyDispenser.coin(unit((0,0))) === false
//    }.eg
//  }
}
