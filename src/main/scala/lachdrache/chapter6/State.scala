package lachdrache.chapter6

case class State[S,+A](run: S => (A,S)) {

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s0 => {
      val (a, s1) = run(s0)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    State(s0 => {
      val (a, s1) = run(s0)
      (f(a), s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s0 => {
      val (a, s1) = run(s0)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

}

object State {

  def unit[S,A](a: A) =
    State[S,A](s => (a,s))

  def sequence[S,A](l: List[State[S,A]]): State[S,List[A]] =
    l.foldRight(unit[S,List[A]](Nil)) { (a,z) =>
      a.map2(z)(_ :: _)
    }

}
