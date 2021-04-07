package com.evolutiongaming.bootcamp.testing

import org.scalacheck.Gen
import org.scalacheck.Test.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params: Parameters = Parameters.default.withMinSuccessfulTests(1000)

  def tupleGen[K, V](firstGen: Gen[K], secondGen: Gen[V]): Gen[(K, V)] = for {
    first  <- firstGen
    second <- secondGen
  } yield (first, second)

  def jsonGen: Gen[Json] = Gen.sized(depth => jsonGen(depth, 8))

  def jsonGen(maxDepth: Int, maxBreadth: Int): Gen[Json] = (maxDepth, maxBreadth) match {
    case (0, _) => jsonPrimitiveGen
    case (d, b) => Gen.oneOf(jsonPrimitiveGen, jsonComplexGen(d, b))
  }

  def jsonComplexGen(maxDepth: Int, maxBreadth: Int): Gen[Json] = for {
    depth   <- Gen.choose(0, maxDepth - 1)
    breadth <- Gen.choose(0, maxBreadth)
    json    <- Gen.oneOf(jsonArrayGen(depth, breadth), jsonObjectGen(depth, breadth))
  } yield json

  def jsonObjectGen(depth: Int, breadth: Int): Gen[JObject] =
    Gen.mapOfN(breadth, tupleGen(Gen.alphaNumStr, jsonGen(depth, breadth))).map(JObject)

  def jsonArrayGen(depth: Int, breadth: Int): Gen[JArray] =
    Gen.containerOfN[Vector, Json](breadth, jsonGen(depth, breadth)).map(JArray)

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
