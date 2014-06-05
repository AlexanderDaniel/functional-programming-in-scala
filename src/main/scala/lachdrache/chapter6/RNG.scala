package lachdrache.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /** Exercise 1
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/1.answer.scala answer]]
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng1)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (n, rng1) = rng.nextInt
    (n<0, rng1)
  }

  /** Exercise 2
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/2.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/2.answer.scala solution]]
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val d = n.toDouble / (Int.MaxValue.toDouble + 1)
    (d, rng2)
  }

  /** Exercise 3
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/3.answer.scala answer]]
    */
  def intDoublePair(rng0: RNG): ((Int, Double), RNG) = {
    val (n, rng1) = rng0.nextInt
    val (d, rng2) = double(rng1)
    ((n,d), rng2)
  }

  def doubleIntPair(rng0: RNG): ((Double, Int), RNG) = {
    val (pair, rng1) = intDoublePair(rng0)
    (pair.swap, rng1)
  }

  def double3(rng0: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng0)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  /** Exercise 4
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/4.answer.scala answer]]
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, acc: (List[Int], RNG)): (List[Int], RNG) =
      (n, acc) match {
        case (0, _) => acc
        case (_, (tail, rng0)) =>
          val (n, rng1) = rng0.nextInt
          go(n-1, (n :: tail, rng1))
      }
    go(count, (Nil, rng))
  }

  /** State action type */
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i%2)

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/5.answer.scala answer]] */
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble+1.0))

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/6.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/6.answer.scala answer]]
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))
  
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequenceWithRecursion[A](rs: List[Rand[A]]): Rand[List[A]] = rs match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequenceWithRecursion(t))(_ :: _)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((a,z) => map2(a,z)(_ :: _))
  
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def notNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else notNegativeLessThan(n)
    }

  /** Exercise 9
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/11.answer.scala answer]]
    */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb)(b => f(a,b))
    }

}
