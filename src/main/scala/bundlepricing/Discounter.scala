package bundlepricing

import bundlepricing.Discounter.{EmptyCart, PartialResult}
import bundlepricing.data.{Bundle, Dollars, Item, Quantity}
import bundlepricing.util.{NonEmptyMap, NonEmptySet, undiscountedTotal}

import scalaz.std.iterable

/**
  * @param products no sense in offering discounts without products
  * @param bundles any available bundles
  */
case class Discounter(products: NonEmptySet[Item], bundles: Set[Bundle]) {

  /**
    * Apply the best combination of bundle savings to the cart
    * @param cart must contain items, to produce a total
    * @return
    */
  def total(cart: NonEmptyMap[Item, Quantity]): Dollars = {
    // import comparison operators
    val quantityOrdering: Ordering[Quantity] = implicitly
    val dollarsOrdering: Ordering[Dollars] = implicitly
    import dollarsOrdering.{mkOrderingOps => dollarsOrderingOps}
    import quantityOrdering.{mkOrderingOps => qtyOrderingOps}

    /** Does the cart contain the bundle's items in sufficient quantity? */
    def canUtilizeBundle(cart: Map[Item, Quantity], bundle: Bundle): Boolean =
      bundle.items.forall {
        case (product, neededQuantity) =>
          cart.get(product).map(cartQuantity => cartQuantity >= neededQuantity).getOrElse(false)
      }

    /** Remove bundle item quantities from a cart */
    def applyBundle(cartRemaining: Map[Item, Quantity], bundle: Bundle): Map[Item, Quantity] = {
      assert(canUtilizeBundle(cartRemaining, bundle))
      bundle.items.foldLeft(cartRemaining) {
        case (cart, (product, quantity)) =>
          if (cart(product) equiv quantity)
            cart - product
          else
            cart.updated(product, cart(product) - quantity)
      }
    }

    def loop(open: List[PartialResult], bestPrice: Dollars): Dollars = {
      open match {
        case Nil =>
          // no more candidate results
          bestPrice

        case PartialResult(EmptyCart(), _, subtotal) :: tail =>
          // no more items in cart
          loop(tail, bestPrice min subtotal)

        case PartialResult(cartRemaining, Nil, subtotal) :: tail =>
          // no more bundles to apply
          loop(tail, bestPrice min (subtotal + undiscountedTotal(cartRemaining)(iterable.iterableSubtypeFoldable)))

        case PartialResult(cartRemaining, bundle :: moreBundles, subtotal) :: tail =>
          def useBundle = // apply this bundle and remove applicable items from the cart
            PartialResult(applyBundle(cartRemaining.toMap, bundle), bundle :: moreBundles, subtotal + bundle.price)
          def ignoreBundle = // don't apply this bundle
            PartialResult(cartRemaining, moreBundles, subtotal)

          if (canUtilizeBundle(cartRemaining, bundle))
            loop(useBundle :: ignoreBundle :: tail, bestPrice)
          else
            loop(ignoreBundle :: tail, bestPrice)
      }
    }

    loop(
      open = List(
        PartialResult(
          cart.toMap,
          bundles.toList.filter(canUtilizeBundle(cart.toMap, _)), // eliminate irrelevant bundles
          Dollars(0)
        )
      ),
      bestPrice = undiscountedTotal(cart.toNEL)
    )
  }
}

object Discounter {

  /** A partially-evaluated cart */
  private case class PartialResult(cartRemaining: Map[Item, Quantity], bundlesRemaining: List[Bundle], subtotal: Dollars)

  /** A pattern matcher for empty Map[Product, Quantity] map */
  private object EmptyCart {
    def unapply(m: Map[Item, Quantity]): Boolean = m.isEmpty
  }
}