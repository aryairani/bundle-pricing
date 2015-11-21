package bundlepricing

import bundlepricing.NonEmptyMapSpec.arbitraryNonEmptyMap
import bundlepricing.data.Dollars.DollarsSyntax
import bundlepricing.data.Quantity.QuantitySyntax
import bundlepricing.data.{Bundle, Item, Quantity}
import bundlepricing.util.{NonEmptyMap, NonEmptySet, undiscountedTotal}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class Examples extends Specification with ScalaCheck { def is = s2"""
    ${ d.total(NonEmptyMap(apple -> 1.pc)) must_== (1.99 $) }
    ${ d.total(NonEmptyMap(apple -> 2.pc)) must_== (2.15 $) }
    ${ d.total(NonEmptyMap(apple -> 3.pc)) must_== (4.14 $) }
    ${ d.total(NonEmptyMap(apple -> 4.pc)) must_== (4.30 $) }
    ${ d.total(NonEmptyMap(apple -> 1.pc, bread -> 2.pc)) must_== (6.89 $) }
    ${ d.total(NonEmptyMap(apple -> 2.pc, bread -> 2.pc, margarine -> 2.pc)) must_== (8.05 $) }
    no bundles means no discounts ${ noBundlesNoDiscounts }
  """

  val apple = Item("apple", 1.99 $)
  val bread = Item("bread", 2.45 $)
  val margarine = Item("margarine", 1.00 $)

  val twoApples = Bundle(2.15 $, apple -> 2.pc)
  val breadButterCombo = Bundle.buyXgetYfree(buy = bread -> 1.pc, margarine -> 1.pc)(getFree = margarine -> 1.pc)

  val d = Discounter(
    products = NonEmptySet(apple, bread, margarine),
    bundles = Set(twoApples, breadButterCombo)
  )

  val noBundles = Discounter(
    products = NonEmptySet(apple, bread, margarine),
    bundles = Set()
  )

  def noBundlesNoDiscounts =
    prop((cart: NonEmptyMap[Item, Quantity]) => noBundles.total(cart) must_== undiscountedTotal(cart.toNEL))

  implicit val arbItem: Arbitrary[Item] = Arbitrary(Gen.oneOf(apple, bread, margarine))
  implicit val arbQuantity: Arbitrary[Quantity] = Arbitrary(Gen.posNum[Int].map(Quantity.apply))
}