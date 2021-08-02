// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.tpolecat.maps

import scala.annotation.unused
import scala.collection.Factory
import scala.collection.immutable.{ TreeMap, TreeSet }
import scala.collection.mutable.Builder
import scala.util.control.NonFatal

/** An ordered, bidirectional one-to-one mapping, where both keys and values are unique. */
final class OneToOne[K, V] private (kv: TreeMap[K, V], vk: TreeMap[V, K]) {

  /**
   * Construct a new `OneToOne` by removing both `k` and `v` if present, then adding the
   * mapping `k -> v`. O(log N).
   * @param k the key
   * @param v the value
   * @return a new `OneToOne` containing the mapping `k -> v`.
   * @group Updating
   */
  def updated(k: K, v: V): OneToOne[K,V] = {
    val vkʹ = if (kv.contains(k)) vk - kv(k) else vk  // remove z where k -> z
    val kvʹ = if (vk.contains(v)) kv - vk(v) else kv  // remove z where z -> v
    new OneToOne(kvʹ + (k -> v), vkʹ + (v -> k))
  }

  /**
   * Construct a new `OneToOn` by removing `k -> ?`, if present. O(log N)
   * @group Updating
   */
  def remove(k: K): OneToOne[K,V] =
    if (kv.contains(k)) new OneToOne(kv - k, vk - kv(k)) else this

  /**
   * The set of keys.
   * @group Querying
   */
  def keys: TreeSet[K] =
    kv.keySet

  /**
   * The set of values.
   * @group Querying
   */
  def values: TreeSet[V] =
    vk.keySet

  /**
   * Shorthand for `getByKey`.
   * @group Querying
   */
  def get(k: K): Option[V] =
    getByKey(k)

  /**
   * The target of the mapping starting at `k`, if any.
   * @group Querying
   */
  def getByKey(k: K): Option[V] =
    kv.get(k)

  /**
   * The target of the mapping starting at `k`, if any.
   * @group Querying
   */
  def getByValue(v: V): Option[K] =
    vk.get(v)

  /**
   * Shorthand for `containsKey`.
   * @group Querying
   */
  def contains(k: K): Boolean =
    containsKey(k)

  /**
   * True if this `OneToOne` contains a mapping from `k`, false otherwise.
   * @group Querying
   */
  def containsKey(k: K): Boolean =
    kv.contains(k)

  /**
   * True if this `OneToOne` contains a mapping to `v`, false otherwise.
   * @group Querying
   */
  def containsValue(v: V): Boolean =
    vk.contains(v)

  /**
   * Invert this `OneToOne` (constant time).
   * @group Manipulation
   */
  def invert: OneToOne[V, K] =
    new OneToOne(vk, kv)

  /**
   * Construct a new `OneToOne` by adding the mappings of `other`, in order.
   * @group Updating
   */
  def ++(other: OneToOne[K, V]): OneToOne[K, V] =
    other.to(List).foldLeft[OneToOne[K, V]](this)(_ + _)

  /**
   * This `OneToOne`, as a standard immutable `Map` (zero cost).
   * @group Conversions
   */
  def toMap: Map[K, V] =
    kv

  /**
   * Convert this `OneToOne` to a collection `F[K, V]` via a standard library `Factory`.
   * @group Conversions
   */
  def to[F[_, _]](f: Factory[(K, V), F[K, V]]): F[K, V] =
    kv.to(f)

  /**
   * Convert this `OneToOne` to a collection `F[(K, V)]` via a standard library `Factory`.
   * @group Conversions
   */
  def to[F[_]](f: Factory[(K, V), F[(K, V)]])(implicit ev: DummyImplicit): F[(K, V)] =
    kv.to(f)

  override def toString(): String =
    kv.addString(new StringBuilder, s"OneToOne(", ", ", ")").toString

  override def equals(obj: Any): Boolean =
    try { sameAs(obj.asInstanceOf[OneToOne[K, V]]) }
    catch { case NonFatal(_) => false }

  override def hashCode: Int =
    kv.hashCode

  private def sameAs(other: OneToOne[K, V]): Boolean =
    to(Iterator).corresponds(other.to(Iterator)) { case ((k, v), (kʹ, vʹ)) =>
      kv.ordering.equiv(k, kʹ) && vk.ordering.equiv(v, vʹ)
    }

  /**
   * Shorthand for `updated`, allowing `m + (k -> v)` syntax.
   * @group Updating
   */
  def +(kv: (K, V)): OneToOne[K, V] =
    updated(kv._1, kv._2)

  /**
   * Shorthand for `remove`.
   * @group Updating
   */
  def -(k: K): OneToOne[K, V] =
    remove(k)

}

/**
 * Companion module for `OneToOne`.
 *
 * In addition to the `.empty` and `.apply` factory methods,
 * this module can be used as a `Factory`, for conversion from standard library collections.
 * {{{
 * Map(1 -> "foo", 2 -> "bar").to(OneToOne) // OneToOne(1 -> foo, 2 -> bar)
 * }}}
 */
object OneToOne {

  /**
   * Construct an empty `OneToOne`.
   * @group Factory Methods
   */
  def empty[K: Ordering, V: Ordering]: OneToOne[K, V] =
    new OneToOne[K, V](TreeMap.empty[K, V], TreeMap.empty[V, K])

  /**
   * Construct a `OneToOne` from the given pairs; `OneToOne(1 -> "foo", 2 -> "bar")`.
   * @group Factory Methods
   */
  def apply[K: Ordering, V: Ordering](entries: (K, V)*): OneToOne[K, V] =
    entries.to(OneToOne)

  /**
   * `OneOnOne` can be used as a `Factory`.
   * @group Implicit Conversions
   */
  implicit def toFactory[K: Ordering, V: Ordering](
    @unused obj: OneToOne.type
  ): Factory[(K, V), OneToOne[K, V]] =
    new Factory[(K, V), OneToOne[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): OneToOne[K, V] =
        it.iterator.foldLeft(empty[K, V])(_ + _)
      def newBuilder: Builder[(K, V),OneToOne[K,V]] =
        new Builder[(K, V), OneToOne[K, V]] {
          var map = empty[K, V]
          def addOne(elem: (K, V)): this.type = { map += elem; this }
          def clear(): Unit = map = empty
          def result(): OneToOne[K,V] = map
        }
    }

}
