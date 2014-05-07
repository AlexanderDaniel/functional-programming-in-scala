package lachdrache.chapter7

trait Par[A]

object Par {
  /** Exercise 1
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.answer.scala answer]]
    */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    ???
}
