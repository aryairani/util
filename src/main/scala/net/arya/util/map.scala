package net.arya.util

import scala.collection.immutable.Map

import monocle.Lens

object map extends MapFunctions

trait MapFunctions {
  def mapWith[A,B](default:B) = Map[A,B]().withDefaultValue(default)
  def mapLensDefault[K, V](k: K, default: V) =
    Lens[Map[K, V],V](_.getOrElse(k,default))(v => _.updated(k,v).withDefaultValue(default))

}
