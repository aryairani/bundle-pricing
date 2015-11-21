package bundlepricing.data

import scalaz.Monoid

/** A non-negative number of dollars. */
case class Dollars(raw: BigDecimal) {
  require(raw >= 0)

  def +(more: Dollars) = Dollars(raw + more.raw)
  def *(qty: Quantity) = Dollars(raw * qty.raw)

  override def toString: String = "$" + raw.toString
}

object Dollars {
  def apply(i: Int): Dollars = Dollars(BigDecimal(i))

  /** Convenience syntax for declaring a lot of prices */
  implicit class DollarsSyntax(d: Double) {
    def $ = Dollars(BigDecimal(d))
  }

  implicit val ordering: Ordering[Dollars] =
    implicitly[Ordering[BigDecimal]].on[Dollars](_.raw)

  implicit val monoid: Monoid[Dollars] = new Monoid[Dollars] {
    def zero: Dollars = Dollars(BigDecimal(0))
    def append(f1: Dollars, f2: => Dollars): Dollars = f1 + f2
  }
}
