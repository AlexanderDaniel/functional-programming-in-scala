package lachdrache.chapter8

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}

object SGen {
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = ???
}
