// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.tpolecat.maps

import scala.collection.immutable.{ TreeMap, TreeSet }

/**
 * A ordered, bidirectioal one-to-many mapping where keys map to disjoint sets of values. This
 * behaves like the inverse of a normal map.
 */
final class OneToMany[K, V](private val kv: TreeMap[K, TreeSet[V]], private val vk: TreeMap[V, K]) {

  // equivelant to removing all mappings from `oldKey` and re-inserting with `newKey`. If the new
  // key happens to be the same as an existig key the mappings are merged.
  def replaceKey(oldKey: K, newKey: K): K = ???



  /** Remove `v` and then add the mapping `k -> v`. */
  def updated(k: K, v: V): OneToMany[K, V] =
    if (apply(k).contains(v)) this
    else {
      val clean = removeValue(v)
      val set   = clean.kv.get(k).fold(TreeSet(v)(vk.ordering))(_ + v)
      new OneToMany(clean.kv + (k -> set), clean.vk + (v -> k))
    }

  def +(kv: (K, V)): OneToMany[K, V] =
    updated(kv._1, kv._2)

  def apply(k: K): Set[V] =
    kv.getOrElse(k, TreeSet.empty[V](vk.ordering))

  /** Remove the mapping from `k -> v`, if present. */
  def remove(k: K, v: V): OneToMany[K, V] =
    kv.get(k) match {
      case None     => this
      case Some(vs) =>
        val s = vs - v
        val kvʹ = if (s.isEmpty) kv - k else kv + (k -> s)
        new OneToMany(kvʹ, vk - v)
    }

  /** Remove all mappings from `k`, if present. */
  def removeKey(k: K): OneToMany[K, V] =
    kv.get(k) match {
      case None     => this
      case Some(vs) => new OneToMany(kv - k, vs.foldLeft(vk)(_ - _))
    }

  /** Remove the mapping to `v`, if present. */
  def removeValue(v: V): OneToMany[K, V] =
    vk.get(v) match {
      case None    => this
      case Some(k) =>
        val s = kv(k) - v
        val kvʹ = if (s.isEmpty) kv - k else kv + (k -> s)
        new OneToMany(kvʹ, vk - v)
    }

  def invert: ManyToOne[V, K] =
    new ManyToOne(vk, kv)

  override def toString: String =
    kv.map { case (k, vs) => s"$k -> ${vs.mkString("{", ", ", "}")}" } .mkString("OneToMany(", ", ", ")")

}

object OneToMany {

  def empty[K: Ordering, V: Ordering]: OneToMany[K, V] =
    new OneToMany(TreeMap.empty, TreeMap.empty)

  def apply[K: Ordering, V: Ordering](es: (K, V)*): OneToMany[K, V] =
    es.foldLeft(empty[K, V])(_ + _)

}