package dev.cptlobster.sc3d
package core

class EulerAngle(val x: Double, val y: Double, val z: Double) extends Point {
  def toArray: Array[Double] = Array(x, y, z)

  /** Return this [[dev.cptlobster.sc3d.core.EulerAngle]] as an [[scala.collection.immutable.Vector]] of [[scala.Double]]s. */
  def toVector: Vector[Double] = Vector(x, y, z)

  /** Return this [[dev.cptlobster.sc3d.core.EulerAngle]] as a [[String]], in format `"(x, y, z)"`. */
  override def toString: String = s"($x, $y, $z)"

  def toVertex: Vertex = Vertex.forward.rotate(new EulerAngle(x, y, z))

  def +(r: EulerAngle): EulerAngle = new EulerAngle(x + r.x, y + r.y, z + r.z)

  def -(r: EulerAngle): EulerAngle = new EulerAngle(x - r.x, y - r.y, z - r.z)

  def *(r: EulerAngle): EulerAngle = new EulerAngle(x * r.x, y * r.y, z * r.z)

  def *(r: Double): EulerAngle = this * new EulerAngle(r, r, r)

  def /(r: EulerAngle): EulerAngle = new EulerAngle(x / r.x, y / r.y, z / r.z)

  def /(r: Double): EulerAngle = this / new EulerAngle(r, r, r)
}

object EulerAngle {
  def apply(x: Double, y: Double, z: Double): EulerAngle = new EulerAngle(x, y, z)
  def radians(x: Double, y: Double, z: Double): EulerAngle = new EulerAngle(x, y, z)
}