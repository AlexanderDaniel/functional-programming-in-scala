package lachdrache.chapter4

sealed trait Option[+A] {
  def map[B](f: A=>B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A=>Option[B]): Option[B] =
    (this map f).getOrElse(None)

  def getOrElse[B >: A](default: => B):B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (v => Some(v)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this flatMap { v => if (f(v)) Some(v) else None }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
