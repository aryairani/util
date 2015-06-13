package net.arya.util

import org.specs2.scalacheck.Parameters
import org.specs2.{Specification, ScalaCheck}

object SetExtractors {
  sealed trait Result
  case object Empty extends Result
  case object NonEmpty extends Result
  case object NoMatch extends Result
  
  def emptySetExtractor[A]: Set[A] ⇒ Result = {
    case EmptySet() ⇒ Empty
    case _ ⇒ NoMatch
  }

  def nonemptySetExtractor[A]: Set[A] ⇒ Result = {
    case NonEmptySet(one, more) ⇒ NonEmpty
    case _ ⇒ NoMatch
  }
}

class SetExtractors extends org.specs2.mutable.Spec {
  import SetExtractors._

  "EmptySet extractor" >> {

    "matches empty Set" >> {
      emptySetExtractor(Set()) mustEqual Empty
    }

    "doesn't match non-empty Set" >> {
      emptySetExtractor(Set(1,2,3)) mustEqual NoMatch
    }
  }

  "NonEmptySet extractor" >> {

    "matches non-empty Set" >> {
      nonemptySetExtractor(Set(1,2,3)) mustEqual NonEmpty
    }

    "doesn't match empty Set" >> {
      nonemptySetExtractor(Set()) mustEqual NoMatch
    }
  }
}

//class SetExtractorProps extends Specification with ScalaCheck {
//  override implicit val defaultParameters = (new Parameters).verbose
//
//  override def is = s2"""
//
//  EmptySet extractor    $empty
//  NonEmptySet extractor $nonempty
//  """
//
//  import SetExtractors._
//  def empty = prop { (s: Set[Int]) ⇒ ((emptySetExtractor(s) == Empty) mustEqual s.isEmpty) }
//  def nonempty = prop { (s: Set[Int]) ⇒ (nonemptySetExtractor(s) == NonEmpty) mustEqual !s.isEmpty }
//}
