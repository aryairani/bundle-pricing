package bundlepricing.data

import bundlepricing.util.{NonEmptyMap, undiscountedTotal}

import scalaz.syntax.semigroup._

case class Bundle(price: Dollars, items: NonEmptyMap[Item, Quantity])

object Bundle {
  def apply(price: Dollars, first: (Item, Quantity), rest: (Item, Quantity)*): Bundle =
    Bundle(price, NonEmptyMap(first, rest: _*))

  def buyXgetYfree(buy: NonEmptyMap[Item, Quantity], getFree: NonEmptyMap[Item, Quantity]): Bundle =
    Bundle(undiscountedTotal(buy.toNEL), buy |+| getFree)

  def buyXgetYfree(buy: (Item, Quantity), andBuy: (Item, Quantity)*)
    (getFree: (Item, Quantity), andGetFree: (Item, Quantity)*): Bundle =
    buyXgetYfree(NonEmptyMap(buy, andBuy: _*), NonEmptyMap(getFree, andGetFree: _*))

}