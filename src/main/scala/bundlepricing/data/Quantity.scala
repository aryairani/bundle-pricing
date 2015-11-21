package bundlepricing.data

import scalaz.Semigroup

/** a positive number of items */
case class Quantity(raw: Int) {
  require(raw > 0)

  def +(q: Quantity): Quantity = Quantity(raw + q.raw)
  def -(q: Quantity): Quantity = Quantity(raw - q.raw)

  override def toString: String = s"${raw}pc"
}

object Quantity {

  /** convenience syntax for producing a quantity from an int */
  implicit class QuantitySyntax(q: Int) {
    def pc = Quantity(q)
  }

  implicit val ordering: Ordering[Quantity] =
    implicitly[Ordering[Int]].on[Quantity](_.raw)

  implicit val semigroup: Semigroup[Quantity] = new Semigroup[Quantity] {
    def append(f1: Quantity, f2: => Quantity): Quantity = f1 + f2
  }
}
