package com.evolutiongaming.bootcamp.basics

import scala.math.{Pi, pow, sqrt}

object ClassesAndTraits {
  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  private[basics] object GeometricUtils {
    // scala.math.hypot works only with two parameters
    // we assume that overflow will not occur here
    // but in general case this method is unsafe
    def hypot(xs: Double*): Double =
      sqrt(xs.fold(0d)((acc, cur) => acc + pow(cur, 2)))

    def heron(ab: Double, bc: Double, ca: Double): Double = {
      val s = (ab + bc + ca) / 2
      sqrt(s * (s - ab) * (s - bc) * (s - ca))
    }
  }

  sealed trait Shape

  // Two dimensional shapes

  sealed trait Shape2D[T <: Shape2D[T]] extends
    Shape with
    Geometric2D with
    Located2D with
    Bounded2D with
    Movable2D[T]

  sealed trait Geometric2D {
    def area: Double
  }

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable2D[T <: Shape2D[T]] {
    def move(dx: Double, dy: Double): T
  }

  object Origin2D extends Located2D {
    val x = 0
    val y = 0
  }

  final case class Point2D(
    x: Double, y: Double
  ) extends Shape2D[Point2D] {

    val minX: Double = x
    val maxX: Double = x
    val minY: Double = y
    val maxY: Double = y
    val area: Double = 0

    def distance(other: Point2D): Double =
      GeometricUtils.hypot(other.x - x, other.y - y)

    def move(dx: Double, dy: Double): Point2D =
      copy(x + dx, y + dy)
  }

  final case class Circle(
    x: Double, y: Double,
    radius: Double
  ) extends Shape2D[Circle] {

    val minX: Double = x - radius
    val maxX: Double = x + radius
    val minY: Double = y - radius
    val maxY: Double = y + radius
    val area: Double = Pi * radius * radius

    def move(dx: Double, dy: Double): Circle =
      copy(x + dx, y + dy)
  }

  final case class Rectangle(
    x: Double, y: Double,
    width: Double, height: Double
  ) extends Shape2D[Rectangle] {

    val minX: Double = x - width / 2
    val maxX: Double = x + width / 2
    val minY: Double = y - height / 2
    val maxY: Double = y + height / 2
    val area: Double = width * height

    def move(dx: Double, dy: Double): Rectangle =
      copy(x + dx, x + dy)
  }

  final case class Square(
    x: Double, y: Double,
    side: Double
  ) extends Shape2D[Square] {

    val minX: Double = x - side / 2
    val maxX: Double = x + side / 2
    val minY: Double = y - side / 2
    val maxY: Double = y + side / 2
    val area: Double = side * side

    def move(dx: Double, dy: Double): Square =
      copy(x + dx, y + dy)
  }

  final case class Triangle(
    a: Point2D, b: Point2D, c: Point2D
  ) extends Shape2D[Triangle] {

    private val xs   = Vector(a, b, c).map(_.x)
    private val ys   = Vector(a, b, c).map(_.y)

    val x: Double    = xs.sum / 3
    val y: Double    = ys.sum / 3

    val minX: Double = xs.min
    val maxX: Double = xs.max
    val minY: Double = ys.min
    val maxY: Double = ys.max

    val area: Double =
      GeometricUtils.heron(a distance b, b distance c, c distance a)

    def move(dx: Double, dy: Double): Triangle =
      copy(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))
  }

  object Bounded2D {
    def minimumBoundingRectangle(objects: Set[Bounded2D]): Bounded2D = {
      new Bounded2D {
        val minX: Double = objects.map(_.minX).min
        val maxX: Double = objects.map(_.maxX).max
        val minY: Double = objects.map(_.minY).min
        val maxY: Double = objects.map(_.maxY).max
      }
    }
  }

  // Three dimensional shapes

  sealed trait Shape3D[T <: Shape3D[T]] extends
    Shape with
    Geometric3D with
    Located3D with
    Bounded3D with
    Movable3D[T]

  sealed trait Geometric3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D[T <: Shape3D[T]] {
    def move(dx: Double, dy: Double, dz: Double): T
  }

  object Origin3D extends Located3D {
    val x = 0
    val y = 0
    val z = 0
  }

  final case class Point3D(
    x: Double, y: Double, z: Double
  ) extends Shape3D[Point3D] {
    val minX: Double = x
    val maxX: Double = x
    val minY: Double = y
    val maxY: Double = y
    val minZ: Double = z
    val maxZ: Double = z

    val surfaceArea: Double = 0
    val volume: Double = 0

    def move(dx: Double, dy: Double, dz: Double): Point3D =
      copy(x + dx, y + dy, z + dz)

    def distance(other: Point3D): Double =
      GeometricUtils.hypot(other.x - x, other.y - y, other.z - z)
  }

  final case class Sphere(
    x: Double, y: Double, z: Double,
    radius: Double
  ) extends Shape3D[Sphere] {

    val minX: Double = x - radius
    val maxX: Double = x + radius
    val minY: Double = y - radius
    val maxY: Double = y + radius
    val minZ: Double = z - radius
    val maxZ: Double = z + radius

    val surfaceArea: Double = 4 * Pi * radius * radius
    val volume: Double = 4 * Pi * pow(radius , 3) / 3

    def move(dx: Double, dy: Double, dz: Double): Sphere =
      copy(x + dx , y + dy , z + dz)
  }

  final case class Cuboid(
    x: Double, y: Double, z: Double,
    width: Double, height: Double, length: Double
  ) extends Shape3D[Cuboid] {

    val minX: Double = x - width / 2
    val maxX: Double = x + width / 2
    val minY: Double = y - height / 2
    val maxY: Double = y + height / 2
    val minZ: Double = z - length / 2
    val maxZ: Double = z + length / 2

    val surfaceArea: Double =
      2 * (length * width + length * height + height * width)
    val volume: Double = width * height * length

    def move(dx: Double, dy: Double, dz: Double): Cuboid =
      copy(x + dx, y + dy, z + dz)
  }

  final case class Cube(
    x: Double, y: Double, z: Double,
    side: Double
  ) extends Shape3D[Cube] {

    val minX: Double = x - side / 2
    val maxX: Double = x + side / 2
    val minY: Double = y - side / 2
    val maxY: Double = y + side / 2
    val minZ: Double = z - side / 2
    val maxZ: Double = z + side / 2

    val surfaceArea: Double = 6 * pow(side, 2)
    val volume: Double = pow(side, 3)

    def move(dx: Double, dy: Double, dz: Double): Cube =
      copy(x + dx, y + dy, z + dz)
  }

  final case class Triangle3D(
    a: Point3D, b: Point3D, c: Point3D
  ) extends Shape3D[Triangle3D] {

    private val xs   = Vector(a, b, c).map(_.x)
    private val ys   = Vector(a, b, c).map(_.y)
    private val zs   = Vector(a, b, c).map(_.z)

    val x: Double    = xs.sum / 3
    val y: Double    = ys.sum / 3
    val z: Double    = zs.sum / 3

    val minX: Double = xs.min
    val maxX: Double = xs.max
    val minY: Double = ys.min
    val maxY: Double = ys.max
    val minZ: Double = zs.min
    val maxZ: Double = zs.max

    val surfaceArea: Double =
      GeometricUtils.heron(a distance b, b distance c, c distance a)
    val volume: Double = 0

    def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      copy(a.move(dx, dy, dz), b.move(dx, dy, dz), c.move(dx, dy, dz))
  }

  object Bounded3D {
    def minimumBoundingCuboid(objects: Set[Bounded3D]): Bounded3D = {
      new Bounded3D {
        val minX: Double = objects.map(_.minX).min
        val maxX: Double = objects.map(_.maxX).max
        val minY: Double = objects.map(_.minY).min
        val maxY: Double = objects.map(_.maxY).max
        val minZ: Double = objects.map(_.minZ).min
        val maxZ: Double = objects.map(_.maxZ).max
      }
    }
  }
}
