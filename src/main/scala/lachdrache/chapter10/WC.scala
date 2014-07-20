package lachdrache.chapter10

import Monoid._

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c+l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r+c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1+l2).isEmpty) 0 else 1) + w2, r2)
    }

    override val zero: WC = Stub("")
  }

  def stringToWc(c: Char): WC =
    if (c.isWhitespace) Part("", 0, "")
    else Stub(c.toString)

  def wc(s: String): WC = {
    foldMapV(s.toIndexedSeq, wcMonoid)(stringToWc)
  }

  def count(s: String): Int = {
    def unstub(s: String) = s.length min 1
    wc(s) match {
      case Stub(c) => unstub(c)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}