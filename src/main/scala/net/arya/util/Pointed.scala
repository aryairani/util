package net.arya.util

import scalaz._

trait Pointed[F[_]] {
  def point[A](a: A): F[A]
}

object Pointed {
  def apply[F[_]](implicit F: Pointed[F]) = F

  implicit class pointedSyntax[A](a: A) {
    def point[F[_]:Pointed]: F[A] = Pointed[F].point(a)
  }

  implicit def applicativePointed[F[_]:Applicative]: Pointed[F] =
    new Pointed[F] {
      override def point[A](a: A): F[A] = Applicative[F].point(a)
    }

  implicit def oneandPointed[F[_]:PlusEmpty]: Pointed[OneAnd[F,?]] = new Pointed[OneAnd[F,?]] {
    override def point[A](a: A): OneAnd[F,A] = OneAnd[F,A](a,PlusEmpty[F].empty[A])
  }

  implicit val setPointed = new Pointed[Set] {
    override def point[A](a: A): Set[A] = Set(a)
  }
}
