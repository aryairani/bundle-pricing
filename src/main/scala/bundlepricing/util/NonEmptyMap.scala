package bundlepricing.util

import scalaz.{Equal, NonEmptyList, Order, Semigroup}

/** An immutable Map with at least one entry */
class NonEmptyMap[K,V] private(raw: Map[K,V]) {
  assert(raw.nonEmpty)

  def get(k: K): Option[V] = raw.get(k)
  def size: Int = raw.size

  /** Remove an element from the map. Returns None if the removed element was the only one. */
  def -(k: K): Option[NonEmptyMap[K, V]] =
    if (raw.size > 1) Some(new NonEmptyMap(raw - k))
    else if (raw.contains(k)) None
    else Some(this)

  /** Add an element to the map, or replace an existing mapping */
  def +(pair: (K, V)): NonEmptyMap[K, V] =
    new NonEmptyMap(raw + pair)

  def ++(other: Map[K, V]): NonEmptyMap[K, V] = new NonEmptyMap(raw ++ other)
  def ++(other: NonEmptyMap[K, V]): NonEmptyMap[K, V] = new NonEmptyMap(raw ++ other.toMap)

  def contains(k: K): Boolean = raw.contains(k)

  def forall(p: ((K, V)) => Boolean) = raw.forall(p)
  def exists(p: ((K, V)) => Boolean) = raw.exists(p)

  def foldLeft[B](z: B)(f: (B, (K, V)) => B): B = raw.foldLeft(z)(f)
  def foldMap1[B](f: ((K,V)) => B)(implicit B: Semigroup[B]): B =
    raw.tail.foldLeft(f(raw.head))((b, kv) => B.append(b, f(kv)))

  def mapValues[C](f: V => C): NonEmptyMap[K, C] = new NonEmptyMap[K, C](raw.mapValues(f))
  def values: NonEmptyList[V] = toNel.map(_._2)

  def toMap: Map[K, V] = raw
  def toList: List[(K, V)] = raw.toList
  def toNel: NonEmptyList[(K, V)] = (raw.toList: @unchecked) match {
    case head :: tail => NonEmptyList.nel(head, tail)
  }

  override def toString: String =
    raw.mkString("NonEmptyMap(", ",", ")")
}

object NonEmptyMap {
  def apply[K, V](one: (K, V), others: (K, V)*): NonEmptyMap[K, V] =
    new NonEmptyMap[K, V](Map(others: _*) + one)

  implicit def semigroup[K, V: Semigroup]: Semigroup[NonEmptyMap[K, V]] =
    Semigroup.instance[NonEmptyMap[K, V]] {
      (a, b) => new NonEmptyMap(scalaz.std.map.mapMonoid[K, V].append(a.toMap, b.toMap))
    }

  implicit def equal[K: Order, V: Equal]: Equal[NonEmptyMap[K, V]] =
    scalaz.std.map.mapEqual[K, V].contramap[NonEmptyMap[K, V]](_.toMap)
}
