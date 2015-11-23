package bundlepricing

import bundlepricing.NonEmptyMapSpec.arbitraryNonEmptyMap
import bundlepricing.data.Dollars.DollarsSyntax
import bundlepricing.data.Quantity.QuantitySyntax
import bundlepricing.data.{Bundle, Dollars, Item, Quantity}
import bundlepricing.util.{NonEmptyMap, undiscountedTotal}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import scalaz.syntax.foldable1._

class Examples extends Specification with ScalaCheck {

  // some items
  val apple = Item("apple", 1.99 $)
  val bread = Item("bread", 2.45 $)
  val margarine = Item("margarine", 1.00 $)
  def items = List(apple, bread, margarine)

  // some bundles
  val twoApples = Bundle(2.15 $, apple -> 2.pc)
  val breadButterCombo = Bundle.buyXgetYfree(buy = bread -> 1.pc, margarine -> 1.pc)(getFree = margarine -> 1.pc)

  // initialize discounter api with bundles
  val d = Discounter(Set(twoApples, breadButterCombo))

  // check some test cases
  def is = s2"""
    ${ check(d, apple -> 1.pc) === (1.99 $) }
    ${ check(d, apple -> 2.pc) === (2.15 $) }
    ${ check(d, apple -> 3.pc) === (4.14 $) }
    ${ check(d, apple -> 4.pc) === (4.30 $) }
    ${ check(d, apple -> 1.pc, bread -> 2.pc) === (6.89 $) }
    ${ check(d, apple -> 2.pc, bread -> 2.pc, margarine -> 2.pc) === (8.05 $) }

    no bundles means no discounts ${
      val noBundles = Discounter(Set())
      prop((cart: NonEmptyMap[Item, Quantity]) => noBundles.total(cart) === undiscountedTotal(cart.toNel))
    }

    if everything was on sale for a penny, n items would cost n cents ${
      val penny = 0.01 $
      val bundles: Set[Bundle] = items.map(item => Bundle(penny, item -> 1.pc)).toSet
      val pennySale = Discounter(bundles)
      prop((cart: NonEmptyMap[Item, Quantity]) => pennySale.total(cart) === penny * cart.values.suml1)
    }
  """

  /** convenience syntax for defining tests */
  def check(d: Discounter, first: (Item, Quantity), rest: (Item, Quantity)*): Dollars =
    d.total(NonEmptyMap(first, rest: _*))

  implicit val arbItem: Arbitrary[Item] = Arbitrary(Gen.oneOf(items))
  implicit val arbQuantity: Arbitrary[Quantity] = Arbitrary(Gen.posNum[Int].map(Quantity.apply))
}