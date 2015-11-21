package bundlepricing.data

case class Item(id: String, price: Dollars) {
  override def toString: String = s"$id @ $price"
}
