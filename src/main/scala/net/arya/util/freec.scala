package net.arya.util

import scalaz._

object freec {
  // thanks, tpolecat

  /**
   * Free monad derivation with correct shape to derive an instance for `Free[Coyoneda[F, ?], ?]`.
   * @group Hacks
   */
  implicit def freeMonadC[FT[_[_], _], F[_]](implicit ev: Functor[FT[F, ?]]) =
    Free.freeMonad[FT[F,?]]

  /**
   * Unapply with correct shape to unpack `Monad[Free[Coyoneda[F, ?], ?]]`.
   * @group Hacks
   */
  implicit def unapplyMMFA[TC[_[_]], M0[_[_], _], M1[_[_], _], F0[_], A0](implicit TC0: TC[M0[M1[F0,?], ?]]):
  Unapply[TC, M0[M1[F0,?], A0]] {
    type M[X] = M0[M1[F0,?], X]
    type A = A0
  } =
    new Unapply[TC, M0[M1[F0,?], A0]] {
      type M[X] = M0[M1[F0,?], X]
      type A = A0
      def TC = TC0
      def leibniz = Leibniz.refl
    }
}
