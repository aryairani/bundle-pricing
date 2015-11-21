package bundlepricing

import bundlepricing.Discounter.{EmptyCart, PartialResult}
import bundlepricing.data.{Bundle, Dollars, Item, Quantity}
import bundlepricing.util.{NonEmptyMap, NonEmptySet, undiscountedTotal}

import scala.annotation.tailrec
import scalaz.std.iterable

/**
  * @param bundles any available bundles
  */
case class Discounter(bundles: Set[Bundle]) {

  /**
    * Apply the best combination of bundle savings to the cart
    * @param cart must contain items, to produce a total
    * @return
    */
  def total(cart: NonEmptyMap[Item, Quantity]): Dollars = {
    // import comparison operators without them shadowing each other
    import Dollars.ordering.{mkOrderingOps => dollarsOrderingOps}
    import Quantity.ordering.{mkOrderingOps => qtyOrderingOps}

    /** Does the cart contain the bundle's items in sufficient quantity? */
    def canApplyBundle(cart: Map[Item, Quantity], bundle: Bundle): Boolean =
      bundle.items.forall {
        case (product, neededQuantity) =>
          cart.get(product).map(cartQuantity => cartQuantity >= neededQuantity).getOrElse(false)
      }

    /** Remove bundle item quantities from a cart */
    def applyBundle(cartRemaining: Map[Item, Quantity], bundle: Bundle): Map[Item, Quantity] = {
      require(canApplyBundle(cartRemaining, bundle), "Don't call applyBundle when canApplyBundle is false.")

      bundle.items.foldLeft(cartRemaining) {
        case (cart, (product, quantity)) =>
          if (cart(product) equiv quantity)
            cart - product
          else
            cart.updated(product, cart(product) - quantity)
      }
    }

    /** try applying combinations of bundles until the lowest price combination is determined */
    @tailrec def loop(open: List[PartialResult], bestPrice: Dollars): Dollars = {
      open match {
        case Nil =>
          // no more candidate results
          bestPrice

        case PartialResult(EmptyCart(), _, subtotal) :: tail =>
          // no more items in cart.  update the bestPrice if ours is better
          loop(tail, bestPrice min subtotal)

        case PartialResult(cartRemaining, Nil, subtotal) :: tail =>
          // no more bundles to apply.  add up the remaining items and update the bestPrice if ours better
          loop(tail, bestPrice min (subtotal + undiscountedTotal(cartRemaining)(iterable.iterableSubtypeFoldable)))

        case PartialResult(cartRemaining, bundle :: moreBundles, subtotal) :: tail =>
          def useBundleScenario = // apply this bundle and remove applicable items from the cart
            PartialResult(applyBundle(cartRemaining, bundle), bundle :: moreBundles, subtotal + bundle.price)

          def ignoreBundleScenario = // don't apply this bundle
            PartialResult(cartRemaining, moreBundles, subtotal)

          if (canApplyBundle(cartRemaining, bundle))
            loop(useBundleScenario :: ignoreBundleScenario :: tail, bestPrice)
          else
            loop(ignoreBundleScenario :: tail, bestPrice)
      }
    }

    /** ignore bundles for things we aren't buying */
    def relevantBundles: Set[Bundle] = bundles.filter(canApplyBundle(cart.toMap, _))

    // start with a full cart and the undiscounted total and begin search
    loop(
      open = List(PartialResult(cart.toMap, relevantBundles.toList, Dollars(0))),
      bestPrice = undiscountedTotal(cart.toNel)
    )
  }
}

object Discounter {
  /** A partially-evaluated cart */
  private case class PartialResult(cartRemaining: Map[Item, Quantity], bundlesRemaining: List[Bundle], subtotal: Dollars)

  /** Pattern matching an empty remaining cart */
  private object EmptyCart { def unapply(m: Map[Item, Quantity]): Boolean = m.isEmpty }
}