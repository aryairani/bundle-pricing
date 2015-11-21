package bundlepricing

import bundlepricing.NonEmptySetSpec.arbitraryNonEmptySet
import bundlepricing.util.NonEmptySet
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

import scalaz.NonEmptyList
import scalaz.scalacheck.ScalazProperties
import scalaz.std.AllInstances._
import scalaz.syntax.std.list._

class NonEmptySetSpec extends Specification with ScalaCheck {
  def is = s2"""
    Semigroup laws ${ ScalazProperties.semigroup.laws[NonEmptySet[Int]] }
    Monad laws ${ ScalazProperties.monad.laws[NonEmptySet] }
    Foldable1 laws ${ ScalazProperties.foldable1.laws[NonEmptySet] }
    removing the only element leaves nothing ${ prop((i: Int) => (NonEmptySet(i) - i).isEmpty) }
    removing 2nd elements leaves something ${
      prop((i1: Int, i2: Int) =>
        (NonEmptySet(i1, i2) - i1).isEmpty === (i1 == i2))
    }
    removing non-member element leaves something ${
      prop((i1: Int, i2: Int) =>
        (NonEmptySet(i1) - i2).isEmpty === (i1 == i2))
    }
    ++(NonEmptySet) ${ prop((a: NonEmptySet[Int], b: NonEmptySet[Int]) => (a ++ b).toSet === (a.toSet ++ b.toSet)) }
    ++(Set) ${ prop((a: NonEmptySet[Int], b: Set[Int]) => (a ++ b).toSet === (a.toSet ++ b)) }
    toNEL consistent with .toList.toNEL ${ prop((a: NonEmptySet[Int]) => a.toNEL === a.toList.toNel.get) }
  """
}

object NonEmptySetSpec {
  implicit def arbitraryNonEmptySet[A: Arbitrary]: Arbitrary[NonEmptySet[A]] = Arbitrary {
    val arb = Arbitrary.arbitrary[A]
    for {
      first <- arb
      rest <- Gen.listOf(arb)
    } yield rest.foldLeft(NonEmptySet(first))((l, a) => l + a)
  }
}