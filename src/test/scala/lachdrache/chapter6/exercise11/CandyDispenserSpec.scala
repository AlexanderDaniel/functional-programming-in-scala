package lachdrache.chapter6.exercise11

import org.specs2.mutable.Specification
import CandyDispenser._
import lachdrache.chapter6.State

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/15.answer.scala answer]]
 */
class CandyDispenserSpec extends Specification {

  testCandyDispenser("my own solution using foldLeft", simulateMachine)
  testCandyDispenser("solution from the authors", simulateMachine_Answer)
  testCandyDispenser("solution from the authors refactored", simulateMachine_AnswerRefactored)

  def testCandyDispenser(name: String, simulateMachine: List[Input] => State[Machine, (Int, Int)]): Unit = {
    name should {
      "empty list of input" should {
        "not change the state" in {
          val machine = simulateMachine(List())
          val initialState = Machine(locked = false, coins = 0, candies = 13)
          machine.run(initialState) ===((0, 13), initialState)
        }
      }

      "Inserting a coin into a locked machine will cause it to unlock if there is any candy left" should {
        "coin into locked machine with left candy" in {
          val machine = simulateMachine(List(Coin))
          val initialState = Machine(locked = true, coins = 0, candies = 13)
          machine.run(initialState) ===((1, 13), Machine(locked = false, coins = 1, candies = 13))
        }
      }

      "Turning the knob on an unlocked machine will cause it to dispense candy and become locked" should {
        "knob on an unlocked machine" in {
          val machine = simulateMachine(List(Turn))
          val initialState = Machine(locked = false, coins = 0, candies = 13)
          machine.run(initialState) ===((0, 12), Machine(locked = true, coins = 0, candies = 12))
        }
      }
    }
  }
}
