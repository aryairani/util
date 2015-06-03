package net.arya.util

import scala.{Boolean, Option, Some, None}
import scala.collection.immutable.Set
import scala.Predef.ArrowAssoc

import scalaz.Maybe._
import scalaz.Ordering._
import scalaz._
import scala.math.Ordering

object foldable extends FoldableFunctions

trait FoldableFunctions {

  /** traverse for States until one returns true */
  // todo: rewrite this to short-circuit
  def findFirstSt[F[_]:Foldable,S,A0](fa: F[A0], st: A0 => State[S,Boolean], s: S): (S,Maybe[A0]) =
    Foldable[F].foldl(fa,(s,empty[A0])) {
      case done @ (_,Just(a)) => a => done
      case looking @ (s,Empty()) => a =>
        val (s2,done) = st(a)(s)
        if (done) (s2,just(a)) else (s2,empty)
    }

  /** traverse for States until one returns true */
  def findFirstSt_[F[_]:Foldable,S,A0](fa: F[A0])(st: A0 => State[S,Boolean])(s: S): Maybe[A0] =
    findFirstSt(fa,st,s)._2

  def findFirstStOrDefault[F[_]:Foldable,S,A0](fa: F[A0], st: A0 => State[S,Boolean], s: S, default: A0): A0 =
    findFirstSt(fa,st,s)._2.getOrElse(default)


  /** return the set of maxima according to f */
  def argmaxesBy[F[_]:Foldable,A,B:Ordering](fa: F[A])(f: A => B): Set[A] =
    Foldable[F].foldl[A,(Set[A],Option[B])](fa,(Set(),None)) {
      case (_,None) =>
        a => Set(a) -> Some(f(a))

      case maxes @ (as, bs @ Some(b0)) =>
        a =>
          val b = f(a)
          fromInt(Ordering[B].compare(b,b0)) match {
            case GT => // a > max
              Set(a) -> Some(b)
            case EQ => // a = max
              (as + a, bs)
            case LT => // a < max
              maxes
          }
    } ._1
}
