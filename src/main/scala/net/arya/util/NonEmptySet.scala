package net.arya.util

import scalaz.Isomorphism.{<~>, IsoFunctorTemplate}
import scalaz._, Scalaz._, Ordering._

object EmptySet {
  def unapply[A](s: Set[A]): Option[Unit] = s.isEmpty.option(())
}

final class NonEmptySet[A] private[util](val head: A, val tail: Set[A]) {

  override def toString = "NonEmpty" + toSet.toString

  def toISet(implicit O: Order[A]): ISet[A] = ISet.fromFoldable(this)
  def toSet: Set[A] = tail + head
  def toList: List[A] = toSet.toList
  def toNEL: NonEmptyList[A] = NonEmptyList(head, tail.toSeq: _*)
  def size = tail.size + 1

  def insert(a: A): NonEmptySet[A] = NonEmptySet(head, tail + a - head)
  def delete(a: A): Set[A] = toSet - a
  def +(a: A): NonEmptySet[A] = insert(a)
  def -(a: A): Set[A] = delete(a)

  def map[B](f: A => B) = {
    val fhead: B = f(head)
    NonEmptySet(fhead, (tail map f) - fhead)
  }

  def flatMap[B](f: A => NonEmptySet[B]): NonEmptySet[B] = {
    val fhead = f(head)
    val ftails = tail map f
    import scalaz.std.set._
    NonEmptySet(fhead.head, fhead.tail union ftails.map(_.toSet).suml - fhead.head)
  }

  def contains(x: A): Boolean = (x == head) || tail.contains(x)
  def toMap[K,V](implicit ev: A <:< (K,V)) = toSet.toMap
  def filter(p: A => Boolean): Set[A] = toSet.filter(p)
}

object NonEmptySet {
  def apply[A](head: A, tail: Set[A]): NonEmptySet[A] =
    new NonEmptySet(head, tail - head)

  def apply[A](head: A, tail: A*): NonEmptySet[A] = apply(head, tail.toSet)

  def unapply[A](s: Set[A]): Option[(A,Set[A])] = s.isEmpty.fold(none, (s.head, s.tail).some)

  def argmaxesBy[F[_],A,B](fa: F[A])(f: A => B)(implicit F: Foldable1[F], ord: math.Ordering[B]): NonEmptySet[A] =
    F.foldMapLeft1[A,(NonEmptySet[A],Option[B])](fa)(a => (NonEmptySet(a),Some(f(a)))) {
      case (maxes @ (as, bs @ Some(b0)), a) =>
        val b = f(a)
        fromInt(ord.compare(b,b0)) match {
          case GT => // a > max
            NonEmptySet(a) -> Some(b)
          case EQ => // a = max
            (as insert a, bs)
          case LT => // a < max
            maxes
        }
    } ._1

  implicit val nonEmptySetInstance: Foldable1[NonEmptySet] with Functor[NonEmptySet] with Pointed[NonEmptySet] with Plus[NonEmptySet] =
    new Foldable1[NonEmptySet] with Functor[NonEmptySet] with Pointed[NonEmptySet] with Plus[NonEmptySet] {
      override def plus[A](a: NonEmptySet[A], b: ⇒ NonEmptySet[A]): NonEmptySet[A] =
        NonEmptySet(a.head, a.tail + b.head ++ b.tail)

      override def foldMap1[A, B](fa: NonEmptySet[A])(f: (A) ⇒ B)(implicit F: Semigroup[B]): B =
        implicitly[Foldable[Set]].foldMap1Opt(fa.toSet)(f).get

      override def foldMapRight1[A, B](fa: NonEmptySet[A])(z: (A) ⇒ B)(f: (A, ⇒ B) ⇒ B): B =
        implicitly[Foldable[Set]].foldMapRight1Opt(fa.toSet)(z)(f).get

      override def map[A, B](fa: NonEmptySet[A])(f: (A) ⇒ B): NonEmptySet[B] =
        NonEmptySet(f(fa.head), fa.tail.map(f))

      override def point[A](a: A): NonEmptySet[A] =
        NonEmptySet(a)
    }

  implicit def nonEmptySetSemigroup[A]: Semigroup[NonEmptySet[A]] = nonEmptySetInstance.semigroup[A]
//  implicit def nonEmptySetShow[A:Show]: Show[NonEmptySet[A]] = foldableShow[NonEmptySet,A]("NESet(", ", ", ")")
  implicit def nonEmptySetOrder[A:Order]: Order[NonEmptySet[A]] = Order[Set[A]].contramap(_.toSet)

//  private def foldableShow[F[_]:Foldable,A:Show](prefix: String, separator: String, suffix: String): Show[F[A]] =
//    Show.show[F[A]] { fa ⇒
//      Cord.stringToCord(prefix) |+|
//        Cord.mkCord(Cord.stringToCord(separator), fa.foldLeft[List[Cord]](Nil)( (l,a) ⇒ a.show :: l ).reverse: _*) |+|
//        Cord.stringToCord(suffix)
//    }

  val oneAndNesIso: NonEmptySet <~> OneAnd[Set,?] =
    new IsoFunctorTemplate[NonEmptySet, OneAnd[Set,?]] {
      def to[A](fa: NonEmptySet[A]): OneAnd[Set,A] = OneAnd[Set,A](fa.head, fa.tail)
      def from[A](ga: OneAnd[Set,A]): NonEmptySet[A] = NonEmptySet(ga.head, ga.tail)
    }
}
