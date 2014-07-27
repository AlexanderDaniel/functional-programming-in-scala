package lachdrache.chapter11

import scala.language.higherKinds

// type constructor
trait Functor[F[_]] {

  def map[A,B](fa: F[A])(f: A => B): F[B]

  // sometimes called unzip
  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: (A) => B): List[B] =
      as map f
  }
}
