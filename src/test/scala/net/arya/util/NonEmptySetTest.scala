package net.arya.util

import org.scalacheck.Arbitrary
import org.specs2.scalaz.Spec
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._

class NonEmptySetTest extends Spec {

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def NonEmptySetArbitrary[A: Arbitrary]: Arbitrary[NonEmptySet[A]] =
    Apply[Arbitrary].apply2(arb[A], arb[List[A]])(NonEmptySet.apply(_, _: _*))

  checkAll("NonEmptySet Order", order.laws[NonEmptySet[Int]])
  checkAll("NonEmptySet Foldable1", foldable1.laws[NonEmptySet])
  checkAll("NonEmptySet Functor", functor.laws[NonEmptySet])
  checkAll("NonEmptySet Plus", plus.laws[NonEmptySet])

}

