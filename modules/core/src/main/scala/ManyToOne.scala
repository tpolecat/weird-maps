// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.tpolecat.maps

import scala.collection.immutable.{ TreeMap, TreeSet }

class ManyToOne[K, V](private val kv: TreeMap[K, V], private val vk: TreeMap[V, TreeSet[K]]) {

  def removeKey(k: K): ManyToOne[K, V] =
    kv.get(k) match {
      case None => this
      case Some(v) =>
        val set = vk(v) - k
        new ManyToOne(kv - k, if (set.isEmpty) vk - v else vk + (v -> set))
    }

  def removeValue(v: V): ManyToOne[K, V] =
    vk.get(v) match {
      case None    => this
      case Some(ks) =>
        new ManyToOne(ks.foldLeft(kv)(_ - _), vk - v)
    }

  def updated(k: K, v: V): ManyToOne[K, V] = {
    val m = removeKey(k)
    val vkʹ = m.vk.get(v) match {
      case None => m.vk + (v -> TreeSet(k)(kv.ordering))
      case Some(ks) => m.vk + (v -> (ks + k))
    }
    new ManyToOne(m.kv + (k -> v), vkʹ)
  }

  def invert: OneToMany[V, K] =
    new OneToMany(vk, kv)

  def +(kv: (K, V)): ManyToOne[K, V] =
    updated(kv._1, kv._2)

  override def toString(): String =
    vk.map { case (v, ks) => s"${ks.mkString("{", ", ", "}")} -> $v" } .mkString("ManyToOne(", ", ", ")")

}

object ManyToOne {

  def empty[K: Ordering, V: Ordering]: ManyToOne[K, V] =
    new ManyToOne(TreeMap.empty, TreeMap.empty)

  def apply[K: Ordering, V: Ordering](es: (K, V)*): ManyToOne[K, V] =
    es.foldLeft(empty[K, V])(_ + _)

}