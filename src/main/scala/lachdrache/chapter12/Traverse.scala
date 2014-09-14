package lachdrache.chapter12

import lachdrache.chapter11.{Monad, Functor}

import scala.language.higherKinds

trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  // exercise c12/14
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)
}

object Traverse {

  // exercise ch12/13 part 1
  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]())) { (a, fbs) =>
        G.map2(f(a), fbs)(_ :: _)
      }
  }

  // exercise ch12/13 part 2
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]] = List())

  // exercise ch12/13 part 3
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

}
