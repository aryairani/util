package net.arya.util

import scalaz._, Scalaz._

object unsafe {

  def unsafeListToNel[A]: List[A] ⇒ NonEmptyList[A] = {
    case head :: tail ⇒ NonEmptyList(head, tail: _*)
    case _ ⇒ throw new IllegalArgumentException("input can't be empty")
  }

  def unsafeListToNes[A]: List[A] ⇒ NonEmptySet[A] = {
    case head :: tail ⇒ NonEmptySet(head, tail.toSet)
    case _ ⇒ throw new IllegalArgumentException("input can't be empty")
  }

  // general utilities
  def atRandom[F[_]:Foldable,A](elems: F[A]): Option[A] = {
    val index = (scala.math.random * elems.count).toInt
    elems.index(index)
  }

  def atRandom1[F[_]:Foldable1,A](elems: F[A]): A = atRandom(elems).get

}
