package bundlepricing.data

/** a named id+price pair */
case class Item(id: String, price: Dollars) {
  override def toString: String = s"$id @ $price"
}
