package lachdrache.chapter13

import java.util.concurrent.atomic.AtomicInteger

object StackOverflowErrorApp extends App {

  val cnt1 = new AtomicInteger(0)
  val cnt2 = new AtomicInteger(0)

  val f = (x: Int) => {
    print(s" ${cnt2.incrementAndGet()}")
    x
  }

  print("building:")
  val g = List.fill(100000)(f).foldLeft(f)((z, a) => {
    print(s" ${cnt1.incrementAndGet()}")
    z compose a
  })
  println()

  try {
    print("running:")
    g(42)
    println()
  }
  catch {
    case e: StackOverflowError =>
      println(s"\nStackOverflowError at ${cnt2.get()}")
  }
}

object AvoidingStackOverflowWithIO2a extends App {
  import IO2a._

  val f: Int => IO[Int] = (x: Int) => Return(x)

  // does not compile TODO
  //  val g = List.fill(100000)(f).foldLeft(f) {
  //    (a: Int => IO[Int], b: Int => IO[Int]) => (x: Int) => Suspend(() => a(x).flatMap(b))
  //  }

}

