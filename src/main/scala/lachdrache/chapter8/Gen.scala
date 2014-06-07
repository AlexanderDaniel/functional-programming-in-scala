package lachdrache.chapter8

import lachdrache.chapter6.{State, RNG}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample flatMap { a =>
      f(a).sample
    })

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { s =>
      Gen.listOfN(s, this)
    }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt) map { n =>
      start + n % (stopExclusive - start)
    })

  def unit[A](a: A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = {
    Gen.choose(from, to) flatMap { a =>
      Gen.choose(from, to) flatMap { b =>
        if (isSameParity(a, b))
          Gen.unit((a, b))
        else
          Gen.unit((a, changeParity(b)))
      }
    }
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean flatMap { b =>
      if (b) g1 else g2
    }

  private def isSameParity(a: Int, b: Int): Boolean =
    even(a)&&even(b) || odd(a)&&odd(b)

  private def even(x: Int): Boolean =
    x%2==0

  private def odd(x: Int): Boolean =
    !even(x)

  private def changeParity(x: Int): Int =
    if (x<0) x+1 else x-1

}
