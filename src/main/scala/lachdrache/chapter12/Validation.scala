package lachdrache.chapter12

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def success[E, A](a: A): Validation[E, A] = Success(a)
  def failure[E, A](head: E, tail: Vector[E] = Vector()): Validation[E,A] = Failure(head, tail)
}
