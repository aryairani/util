package net.arya.util

import scalaz._, Scalaz._
import Pointed.pointedSyntax

object filter {

  // todo: refactor these ones that make assumptions about the input function

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1Nel[F[_]:Foldable1,A](input: F[A])(test: A ⇒ Boolean): NonEmptyList[A] =
    filter1FG[F,NonEmptyList,A](input)(test)

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1FF[F[_]:Foldable1:Pointed:Plus,A](input: F[A])(test: A ⇒ Boolean): F[A] =
    filter1FG[F,F,A](input)(test)

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1FG[F[_]:Foldable1,G[_]:Foldable1:Pointed:Plus,A]
  (input: F[A])(test: A ⇒ Boolean): G[A] =
    input.foldMap1(_.point[G])(Semigroup.instance[G[A]](
      (g1, g2) ⇒
        (g1.all(test), g2.all(test)) match {
          case (true, true) ⇒ g1 <+> g2
          case (true, false) ⇒ g1
          case (false, true) ⇒ g2
          case _ ⇒ g1 // either; it should get thrown away
        }
    )) <| (result ⇒ assert(result.all(test), "test returned false for all inputs"))

  def filterFG[F[_]:Foldable,G[_]:Pointed:PlusEmpty,A]
  (input: F[A])(test: A ⇒ Boolean): G[A] =
    input.foldMap[G[A]]( a ⇒ test(a).fold[G[A]](a.point[G], PlusEmpty[G].empty) )(PlusEmpty[G].monoid)

  def filterFF[F[_]:Foldable:Pointed:PlusEmpty,A]
  (input: F[A])(test: A ⇒ Boolean): F[A] =
    input.foldMap[F[A]]( a ⇒ test(a).fold[F[A]](a.point[F], PlusEmpty[F].empty) )(PlusEmpty[F].monoid)

}
