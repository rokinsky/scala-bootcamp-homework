package com.evolutiongaming.bootcamp.testing

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params: Parameters = Parameters.default.withMinSuccessfulTests(1000)

  def tupleGen[K, V](firstGen: Gen[K], secondGen: Gen[V]): Gen[(K, V)] =
    for {
      first  <- firstGen
      second <- secondGen
    } yield (first, second)

  def jsonGen: Gen[Json] = Gen.sized(jsonGen)

  def jsonGen(maxDepth: Int): Gen[Json] = maxDepth match {
    case 0 => jsonPrimitiveGen
    case n => Gen.oneOf(jsonPrimitiveGen, jsonComplexGen(n))
  }

  def jsonComplexGen(maxDepth: Int): Gen[Json] = for {
    depth <- Gen.choose(0, maxDepth - 1)
    json  <- Gen.oneOf(jsonArrayGen(depth), jsonObjectGen(depth))
  } yield json

  def jsonObjectGen(maxDepth: Int): Gen[JObject] =
    Gen.mapOfN(4, tupleGen(Gen.alphaNumStr, jsonGen(maxDepth))).map(JObject)

  def jsonArrayGen(maxDepth: Int): Gen[JArray] =
    Gen.containerOfN[Vector, Json](4, jsonGen(maxDepth)).map(JArray)

  def jsonPrimitiveGen: Gen[Json] =
    Gen.oneOf(Gen.const(JNull), jsonBooleanGen, jsonNumberGen, jsonStringGen)

  def jsonBooleanGen: Gen[JBoolean] = Gen.oneOf(true, false).map(JBoolean)
  def jsonNumberGen:  Gen[JNumber]  = Gen.double.map(JNumber)
  def jsonStringGen:  Gen[JString]  = Gen.alphaNumStr.map(JString)

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      assert(parse(print(json)) === Some(json))
    }
  }
}
