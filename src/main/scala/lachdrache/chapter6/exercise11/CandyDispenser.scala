package lachdrache.chapter6.exercise11

import lachdrache.chapter6.State
import lachdrache.chapter6.State._

object CandyDispenser {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, (Int, Int)](s => ((s.coins, s.candies), s))) {
      (z, in) => update(z)(in match {
        case Coin => m => if (m.candies > 0 && m.locked) m.copy(locked = false, coins = m.coins + 1) else m
        case Turn => m => if (!m.locked) m.copy(locked = true, candies = m.candies - 1) else m
        case _ => identity
      })
    }

  def update(z: State[Machine, (Int, Int)])(f: Machine => Machine): State[Machine, (Int, Int)] =
    z.flatMap(_ => modify(f)).flatMap(_ => get).map(m => (m.coins, m.candies))

  def simulateMachine_Answer(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- get
  } yield (s.coins, s.candies)

  def simulateMachine_AnswerRefactored(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map inputToState)
      s <- get
    } yield (s.coins, s.candies)


  def inputToState(i: Input): State[Machine, Unit] =
    modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })
}
