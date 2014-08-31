package lachdrache.chapter12

import scala.language.higherKinds

trait Traverse[F[_]]  {

  def traverse[G[_]:Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
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

  case class Tree[+A](head: A, tail: List[Tree[A]])

  // exercise ch12/13 part 2
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

}
