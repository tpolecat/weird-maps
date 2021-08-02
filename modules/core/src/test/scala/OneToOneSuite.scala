// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.tpolecat.maps

import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable.TreeMap

class OneToOneSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
         .withMinSuccessfulTests(1000)

  val key:   Gen[Int] = Gen.choose(1, 20)
  val value: Gen[Char] = Gen.alphaLowerChar
  val entry: Gen[(Int, Char)] = key.flatMap(k => value.map(k -> _))
  val map:   Gen[OneToOne[Int, Char]] = Gen.listOfN(100, entry).map(_.to(OneToOne))

  // Conversions

  test(".to(List).to(OneToOne) is an identity") {
    forAll(map) { m =>
      assertEquals(m.to(List).to(OneToOne), m)
    }
  }

  test(".to(TreeMap).to(OneToOne) is an identity, assuming coherent ordering") {
    forAll(map) { m =>
      assertEquals(m.to(TreeMap).to(OneToOne), m)
    }
  }

  test(".toMap.to(OneToOne) is an identity") {
    forAll(map) { m =>
      assertEquals(m.toMap.to(OneToOne), m)
    }
  }

  // Manipulation

  test("inverting twice is an identity") {
    forAll(map) { m =>
      assertEquals(m.invert.invert, m)
    }
  }

  // Querying

  test("contains is consistent with containsKey") {
    forAll(map, key) { (m, k) =>
      assertEquals(m.contains(k), m.containsKey(k))
    }
  }

  test("containsKey is consistent with getByKey") {
    forAll(map, key) { (m, k) =>
      assertEquals(m.containsKey(k), m.getByKey(k).isDefined)
    }
  }

  test("containsValue is consistent with getByValue") {
    forAll(map, value) { (m, v) =>
      assertEquals(m.containsValue(v), m.getByValue(v).isDefined)
    }
  }

  test("get is consistent with getByKey") {
    forAll(map, key) { (m, k) =>
      assertEquals(m.get(k), m.getByKey(k))
    }
  }

  test("getByKey after updated") {
    forAll(map, key, value) { (m, k, v) =>
      assertEquals((m + (k -> v)).getByKey(k), Some(v))
    }
  }

  test("getByValue after updated") {
    forAll(map, key, value) { (m, k, v) =>
      assertEquals((m + (k -> v)).getByValue(v), Some(k))
    }
  }

  test("removed key can't be queried by key") {
    forAll(map, key, value) { (m, k, v) =>
      val mʹ = m + (k -> v)
      val mʹʹ = mʹ - k
      assertEquals(mʹʹ.getByKey(k), None)
    }
  }

  test("removed key can't be queried by value") {
    forAll(map, key, value) { (m, k, v) =>
      val mʹ = m + (k -> v)
      val mʹʹ = mʹ - k
      assertEquals(mʹʹ.getByValue(v), None)
    }
  }

}

