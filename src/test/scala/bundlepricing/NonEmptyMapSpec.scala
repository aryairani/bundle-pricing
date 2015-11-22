package bundlepricing

import bundlepricing.NonEmptyMapSpec.arbitraryNonEmptyMap
import bundlepricing.util.NonEmptyMap
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

import scalaz.{Foldable1, NonEmptyList}
import scalaz.scalacheck.ScalazProperties
import scalaz.std.AllInstances._
import scalaz.syntax.std.list._

class NonEmptyMapSpec extends Specification with ScalaCheck {
  def is = s2"""
    Semigroup laws ${ ScalazProperties.semigroup.laws[NonEmptyMap[Int, Int]] }
    removing the only element leaves nothing ${ prop((k: Int, v: Int) => (NonEmptyMap(k -> v) - k).isEmpty) }
    removing 2nd elements leaves something ${
      prop((k1: Int, v1: Int, k2: Int, v2: Int) =>
        (NonEmptyMap(k1 -> v1, k2 -> v2) - k1).isEmpty === (k1 == k2))
    }
    removing non-member element leaves something ${
      prop((k1: Int, v1: Int, k2: Int) =>
        (NonEmptyMap(k1 -> v1) - k2).isEmpty === (k1 == k2))
    }
    foldMap1 consistent with NonEmptyList ${
      prop((m: NonEmptyMap[Int,String]) =>
        m.foldMap1(Vector(_)) === Foldable1[NonEmptyList].foldMap1(m.toNel)(Vector(_))
      )
    }
    ++(NonEmptyMap) ${ prop((a: NonEmptyMap[Int,String], b: NonEmptyMap[Int,String]) => (a ++ b).toMap === (a.toMap ++ b.toMap)) }
    ++(Map) ${ prop((a: NonEmptyMap[Int,String], b: Map[Int,String]) => (a ++ b).toMap === (a.toMap ++ b)) }
    .toNel consistent with .toList.toNel ${ prop((a: NonEmptyMap[Int,String]) => a.toNel === a.toList.toNel.get) }
    .mapValues.toMap consistent with .toMap.mapValues ${
      prop((a: NonEmptyMap[Int,Int]) => a.mapValues(_+1).toMap === a.toMap.mapValues(_+1))
    }
  }
  """
}

object NonEmptyMapSpec {
  implicit def arbitraryNonEmptyMap[K: Arbitrary, V: Arbitrary]: Arbitrary[NonEmptyMap[K, V]] = Arbitrary {
    val pair: Gen[(K, V)] = Arbitrary.arbitrary[(K, V)]
    for {
      first <- pair
      rest <- Gen.listOf(pair)
    } yield rest.foldLeft(NonEmptyMap(first))((l, kv) => l + kv)
  }
}