package lachdrache.chapter14

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st[Unit].run(())._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  def write(i: Int, a: A): ST[S,Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)

  def noop: ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      ((), s)
    }
  }

  // Exercise 14.1
  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    xs.foldLeft(ST[S,Unit](())) { case (z, (k, v)) =>
      z flatMap (_ => write(k, v))
    }
  }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      iv <- read(i)
      jv <- read(j)
      _ <- write(i, jv)
      _ <- write(j, iv)
    } yield ()

}

object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })

}

object QuicksortUsingST {
  def noop[S] = ST[S,Unit](())

  // Exercise 14.2
  def partition[S](arr: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    vp <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- arr.read(i)
      _  <- if (vi < vp) (for {
        vj <- j.read
        _  <- arr.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield ()) else noop[S]
    } yield ())
    x <- j.read
    _ <- arr.swap(x, r)
  } yield x

  // Exercise 14.2
  def qs[S](arr: STArray[S,Int], l: Int, r: Int): ST[S,Unit] =
    if (l < r) {
      for {
        pi <- partition(arr, l, r, l + (r - l) / 2)
        _ <- qs(arr, l, pi -1)
        _ <- qs(arr, pi + 1, r)
      } yield ()
    } else {
      noop[S]
    }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      override def apply[S]: ST[S, List[Int]] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

}

// Exercise 14.3
sealed abstract class STMap[S, K, V] {
  protected def value: scala.collection.mutable.HashMap[K,V]

  def size: ST[S,Int] = ST(value.size)

  def put(k: K, v: V): ST[S,Unit] =
    ST(value.put(k, v))

  def +=(kv :(K,V)): ST[S,Unit] =
    ST(value += kv)

  def -=(k: K): ST[S,Unit] =
    ST(value -= k)

  def get(k: K): ST[S, Option[V]] =
    ST(value.get(k))

  def freeze: ST[S,Map[K,V]] =
    ST(value.toMap)
}

object STMap {
  def empty[S,K,V]: ST[S, STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      val value = scala.collection.mutable.HashMap.empty[K,V]
    })

  def fromMap[S,K,V](m: Map[K,V]): ST[S, STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      val value = (scala.collection.mutable.HashMap.newBuilder[K,V] ++= m).result
    })
}


