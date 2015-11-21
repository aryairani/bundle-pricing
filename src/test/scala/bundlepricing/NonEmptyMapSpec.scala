package bundlepricing

import bundlepricing.NonEmptyMapSpec.arbitraryNonEmptyMap
import bundlepricing.util.NonEmptyMap
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

import scalaz.scalacheck.ScalazProperties
import scalaz.std.AllInstances._

class NonEmptyMapSpec extends Specification with ScalaCheck {
  def is = s2"""
    NonEmptyMap obeys Semigroup laws ${ ScalazProperties.semigroup.laws[NonEmptyMap[Int, Int]] }
  """
}

object NonEmptyMapSpec {
  implicit def arbitraryNonEmptyMap[K:Arbitrary,V:Arbitrary]: Arbitrary[NonEmptyMap[K,V]] = Arbitrary {
    val pair: Gen[(K,V)] = Arbitrary.arbitrary[(K,V)]
    for {
      first <- pair
      rest <- Gen.listOf(pair)
    } yield rest.foldLeft(NonEmptyMap(first))( (l,kv) => l + kv )
  }
}