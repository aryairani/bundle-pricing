package bundlepricing

import bundlepricing.data.{Dollars, Item, Quantity}

import scalaz.Foldable

package object util {

  def undiscountedTotal[F[_]](products: F[(Item, Quantity)])(implicit F: Foldable[F]): Dollars =
    F.foldMap(products) { case (product, quantity) => product.price * quantity }
}
