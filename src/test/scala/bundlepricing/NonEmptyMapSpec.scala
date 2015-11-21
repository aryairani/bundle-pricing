package bundlepricing

import bundlepricing.NonEmptyMapSpec.arbitraryNonEmptyMap
import bundlepricing.util.NonEmptyMap
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

import scalaz.scalacheck.ScalazProperties
import scalaz.std.AllInstances._

class NonEmptyMapSpec extends Specification with ScalaCheck {
  def is = s2"""
    Semigroup laws ${ ScalazProperties.semigroup.laws[NonEmptyMap[Int, Int]] }
    deleting the only element leaves nothing ${ prop((k: Int, v: Int) => (NonEmptyMap(k -> v) - k).isEmpty) }
    deleting some elements leaves somthing ${
      prop((k1: Int, v1: Int, k2: Int, v2: Int) =>
        (NonEmptyMap(k1 -> v1, k2 -> v2) - k1).isEmpty === (k1 == k2)) }
    deleting some elements leaves somthing ${
      prop((k1: Int, v1: Int, k2: Int) =>
        (NonEmptyMap(k1 -> v1) - k2).isEmpty === (k1 == k2)) }
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