package bundlepricing.util

/** An immutable Set with at least one element */
class NonEmptySet[A] private(raw: Set[A]) {
  /** Remove an element from the set. Returns None if the removed element was the only one. */
  def -(k: A): Option[NonEmptySet[A]] =
    if (raw.size > 1) Some(new NonEmptySet(raw - k))
    else if (raw.contains(k)) None
    else Some(this)

  /** Add an element to the set */
  def +(a: A): NonEmptySet[A] =
    new NonEmptySet(raw + a)

  def contains(a: A): Boolean = raw.contains(a)

  def toSet: Set[A] = raw
  def toList: List[A] = raw.toList
}

object NonEmptySet {
  def apply[A](one: A, others: A*): NonEmptySet[A] =
    new NonEmptySet[A](Set(others: _*) + one)
}

