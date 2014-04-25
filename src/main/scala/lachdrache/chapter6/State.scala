package lachdrache.chapter6

import State._

case class State[S,+A](run: S => (A,S)) {

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s0 => {
      val (a, s1) = run(s0)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

}

object State {

  def unit[S,A](a: A): State[S,A] =
    State(s => (a,s))

  def sequence[S,A](l: List[State[S,A]]): State[S,List[A]] =
    l.foldRight(unit[S,List[A]](Nil)) { (a,z) =>
      a.map2(z)(_ :: _)
    }

  def get[S]: State[S,S] =
    State(s => (s,s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}
