package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.{Pi, pow}

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  import ClassesAndTraits._

  behavior of "Origin2D"

  it should "have correct coordinates" in {
    Origin2D.x shouldEqual 0.0
    Origin2D.y shouldEqual 0.0
  }

  behavior of "Point2D"

  it should "have correct coordinates" in {
    forAll {
      (a: Double, b: Double) => {
        val point = Point2D(a, b)
        point.x shouldEqual a
        point.y shouldEqual b
      }
    }
  }

  it should "have correct area" in {
    forAll {
      (a: Double, b: Double) => {
        Point2D(a, b).area shouldEqual 0
      }
    }
  }

  it should "move correctly" in {
    val point = Point2D(0, 0)
    forAll {
      (a: Double, b: Double) => {
        point.move(a, b) shouldEqual Point2D(a, b)
        point.move(0, b).move(a, 0) shouldEqual Point2D(a, b)
        point.move(a, 0).move(0, b) shouldEqual Point2D(a, b)
        point.move(a, b).move(-a, 0) shouldEqual Point2D(0, b)
        point.move(a, b).move(0, -b) shouldEqual Point2D(a, 0)
      }
    }
  }

  behavior of "Circle"

  it should "move correctly" in {
    val circle = Circle(0, 0, 10)
    circle.move(4, 0).x shouldEqual 4
    circle.move(1, 0).move(1, 0).x shouldEqual 2
    circle.move(4, 5).y shouldEqual 5

    Circle(3, 5, 4).move(5,0).x shouldEqual 8
  }

  it should "have correct area" in {
    Circle(3, 2, 5).area shouldEqual 78.54 +- 0.001
    Circle(1, 1, 4).area shouldEqual Pi * pow(4, 2)
    Circle(10, 5, 3.3).area shouldEqual 34.21 +- 0.1
  }

  behavior of "Rectangle"

  it should "move correctly" in {
    val rect = Rectangle(0, 0, 3.4, 9.5)
    rect.move(3, 0).x shouldEqual 3
    rect.move(3, 5).y shouldEqual 5
  }

  it should "have correct area" in {
    Rectangle(3.2, 4.5, 5, 10).area shouldEqual 50
    Rectangle(1, 1, 8, 4).area shouldEqual 32
  }

  behavior of "Square"

  it should "move correctly" in {
    val square = Square(0, 0, 7)
    square.move(3, 0).move(-1, 0).x shouldEqual 2
    square.move(3, 5).y shouldEqual 5
  }

  it should "have correct area" in {
    Square(0, 0, 7).area shouldEqual 49
    Square(1, 1, 4).area shouldEqual 16
  }

  behavior of "Triangle"

  it should "be centered correctly" in {
    val triangle = Triangle(Point2D(0, 0), Point2D(5, 0), Point2D(5, 10))
    triangle.x shouldEqual 3.333 +- 0.001
    triangle.y shouldEqual 3.333 +- 0.001
  }

  it should "move correctly" in {
    val triangle = Triangle(Point2D(0, 0), Point2D(5, 0), Point2D(5, 10))
    triangle.move(5, 0).x shouldEqual 8.333 +- 0.001
    triangle.move(0, 5).x shouldEqual 3.333 +- 0.001
  }

  it should "have correct area" in {
    val triangle1 = Triangle(Point2D(0, 0), Point2D(5, 0), Point2D(5, 10))
    triangle1.area shouldEqual 25.0 +- 0.001

    val triangle2 = Triangle(Point2D(1, 1), Point2D(2, 1), Point2D(1, 2))
    triangle2.area shouldEqual 0.5 +- 0.001
  }

  behavior of "Bounded2D"

  it should "correct recognize minimum bounding rectangle" in {
    val mbr = Bounded2D.minimumBoundingRectangle(
      Set(
        Point2D(-12, -3),
        Point2D(-3, 7),
        Circle(0, 0, 5),
      )
    )

    mbr.minX shouldEqual -12
    mbr.maxX shouldEqual 5
    mbr.minY shouldEqual -5
    mbr.maxY shouldEqual 7
  }

  behavior of "Origin3D"

  it should "have correct coordinates" in {
    Origin3D.x shouldEqual 0.0
    Origin3D.y shouldEqual 0.0
    Origin3D.z shouldEqual 0.0
  }

  behavior of "Point3D"

  it should "have correct coordinates" in {
    forAll {
      (a: Double, b: Double, c: Double) => {
        val point = Point3D(a, b, c)
        point.x shouldEqual a
        point.y shouldEqual b
        point.z shouldEqual c
      }
    }
  }

  it should "have correct surface area" in {
    forAll {
      (a: Double, b: Double, c: Double) => {
        Point3D(a, b, c).surfaceArea shouldEqual 0
      }
    }
  }

  it should "move correctly" in {
    val point = Point3D(0, 0, 0)
    forAll {
      (a: Double, b: Double, c: Double) => {
        point.move(a, b, c) shouldEqual Point3D(a, b, c)
        point.move(a, b, c).move(-a, 0, -c) shouldEqual Point3D(0, b, 0)
      }
    }
  }

  behavior of "Sphere"

  it should "have correct surface area" in {
    Sphere(3, 4, 5, 10).surfaceArea shouldEqual 1256.63 +- 0.1
    Sphere(1,1,1,4).surfaceArea shouldEqual 201.06 +- 0.01
    Sphere(0, 0, 0, 10).surfaceArea shouldEqual 1256.637 +- 0.001
  }

  it should "have correct volume" in {
    Sphere(3, 3, 3, 3.6278317).volume shouldEqual 200.0 +- 0.00001
    Sphere(1,1,1,4).volume shouldEqual 268.08 +- 0.01
    Sphere(0, 0, 0, 10).volume shouldEqual 4188.79 +- 0.001
  }

  it should "move correctly" in {
    val sphere = Sphere(0, 0, 0, 10)
    sphere.move(6, 0, 0).move(1, 0, 0).x shouldEqual 7
    sphere.move(0, 0, 5).y shouldEqual 0
    sphere.move(0, 0, 5).z shouldEqual 5
  }

  behavior of "Cuboid"

  it should "have correct surface area" in {
    Cuboid(1, 1, 1, 8, 4, 6).surfaceArea shouldEqual 208.0 +- 0.01
    Cuboid(0, 0, 0, 15, 12, 10).surfaceArea shouldEqual 900
  }

  it should "have correct volume" in {
    Cuboid(1, 1, 1, 8, 4, 6).volume shouldEqual 192.0 +- 0.01
    Cuboid(0, 0, 0, 15, 12, 10).volume shouldEqual 1800
  }

  it should "move correctly" in {
    val cuboid = Cuboid(0, 0, 0, 15, 12, 10)
    cuboid.move(6, 0, 0).move(1, 0, 0).x shouldEqual 7
    cuboid.move(0, 0, 5).y shouldEqual 0
    cuboid.move(0, 0, 5).z shouldEqual 5
  }

  behavior of "Cube"

  it should "have correct surface area" in {
    Cube(1, 1, 1, 8).surfaceArea shouldEqual 384.0 +- 0.01
    Cube(0, 0, 0, 15).surfaceArea shouldEqual 1350
  }

  it should "have correct volume" in {
    Cube(1, 1, 1, 8).volume shouldEqual 512.0 +- 0.01
    Cube(3, 3, 3, 3).volume shouldEqual 3 * 3 * 3
    Cube(0, 0, 0, 15).volume shouldEqual 3375
  }

  it should "move correctly" in {
    val cube = Cube(0, 0, 0, 15)
    cube.move(6, 0, 0).move(1, 0, 0).x shouldEqual 7
    cube.move(0, 0, 5).y shouldEqual 0
    cube.move(0, 0, 5).z shouldEqual 5
  }

  behavior of "Triangle3D"

  it should "be centered correctly" in {
    val triangle = Triangle3D(Point3D(0, 0, 0), Point3D(5, 0, 0), Point3D(5, 10, 0))
    triangle.x shouldEqual 3.333 +- 0.001
    triangle.y shouldEqual 3.333 +- 0.001
    triangle.z shouldEqual 0.0 +- 0.001
  }

  it should "move correctly" in {
    val triangle = Triangle3D(Point3D(0, 0, 0), Point3D(5, 0, 0), Point3D(5, 10, 0))
    triangle.move(5, 0, 0).x shouldEqual 8.333 +- 0.001
    triangle.move(0, 5, 0).x shouldEqual 3.333 +- 0.001
  }

  it should "have correct area" in {
    val triangle1 = Triangle3D(Point3D(0, 0, 0), Point3D(5, 0, 0), Point3D(5, 10, 0))
    triangle1.surfaceArea shouldEqual 25.0 +- 0.001

    val triangle2 = Triangle3D(Point3D(1,1, 0), Point3D(2, 1, 0), Point3D(1, 2, 0))
    triangle2.surfaceArea shouldEqual 0.5 +- 0.001
  }

  behavior of "Bounded3D"

  it should "correct recognize minimum bounding cuboid" in {
    val mbb = Bounded3D.minimumBoundingCuboid(Set(
      Point3D(-12, -3, 4),
      Point3D(1, 7, -4.5),
      Sphere(4, 0, 5, 5),
    ))

    mbb.minX shouldEqual -12
    mbb.maxX shouldEqual 9
    mbb.minY shouldEqual -5
    mbb.maxY shouldEqual 7
    mbb.minZ shouldEqual -4.5
    mbb.maxZ shouldEqual 10
  }

  import GeometricUtils._

  behavior of "hypot"

  it should "be correct for eleven arguments" in {
    hypot(1, 3, 2, 6, 1, 7, 3, 7, 5, 3, 1) shouldEqual 13.892 +- 0.1
  }

  it should "be correct for three arguments" in {
    hypot(3, 4, 5) shouldEqual 7.071 +- 0.1
  }

  it should "be correct for two positive arguments" in {
    hypot(3, 4) shouldEqual 5
  }

  it should "be correct for two negative arguments" in {
    hypot(-5, -12) shouldEqual 13
  }

  it should "be correct for one argument" in {
    hypot(-5) shouldEqual 5
  }

  it should "be correct for zero arguments" in {
    hypot() shouldEqual 0
  }

  behavior of "heron"

  it should "be correct for triangle with integer sides" in {
    heron(4, 5, 6) shouldEqual 9.9 +- 0.1
  }

  it should "be correct for triangle with double sides" in {
    heron(3.16, 5.83, 4.47) shouldEqual 7.0 +- 0.1
  }

  it should "be correct for triangle with zero-length sides" in {
    heron(0, 0, 0) shouldEqual 0.0 +- 0.1
  }
}
