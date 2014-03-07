package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/** Can short-circuiting work with [[foldRight]] like
  * we have done it in [[product]]?
  *
  * I argue no because the recursion is done by [[foldRight]]
  * and we have no chance to alter this behaviour with the
  * passed in function.
  *
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/7.hint.txt Hint]] and
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/7.answer.scala Answer]]
  */
class Exercise7Spec extends Specification {

}