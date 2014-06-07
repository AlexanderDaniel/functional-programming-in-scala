package lachdrache.chapter8

import lachdrache.chapter8.Prop._

case class Prop(run: TestCases => Result)

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
}
