/*
 *   Scala 3D renderer - Euler Angle classes
 *   Copyright (C) 2022-2023 Dustin Thomas
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
package dev.cptlobster.sc3d
package core

/** Basic class for representing Euler angles in 3D space.
 *
 * @note All instances use [[scala.Double]]s to store values.
 *
 * @constructor Create a new Vertex point.
 * @param x Angle around the x-axis (roll)
 * @param y Angle around the y-axis (pitch)
 * @param z Angle around the z-axis (yaw)
 */

class EulerAngle(val x: Double, val y: Double, val z: Double) {
  def toArray: Array[Double] = Array(x, y, z)

  /** Return this [[dev.cptlobster.sc3d.core.EulerAngle]] as an [[scala.collection.immutable.Vector]] of [[scala.Double]]s. */
  def toVector: Vector[Double] = Vector(x, y, z)

  /** Return this [[dev.cptlobster.sc3d.core.EulerAngle]] as a [[String]], in format `"(x, y, z)"`. */
  override def toString: String = s"($x, $y, $z)"

  /** Convert this [[dev.cptlobster.sc3d.core.EulerAngle]] into a [[dev.cptlobster.sc3d.core.Vertex]], with the same
   * direction as this angle, and a magnitude of 1. */
  def toVertex: Vertex = Vertex.forward.rotate(new EulerAngle(x, y, z))

  def +(r: EulerAngle): EulerAngle = new EulerAngle(x + r.x, y + r.y, z + r.z)

  def -(r: EulerAngle): EulerAngle = new EulerAngle(x - r.x, y - r.y, z - r.z)
}

object EulerAngle {
  def apply(x: Double, y: Double, z: Double): EulerAngle = new EulerAngle(x, y, z)
  def radians(x: Double, y: Double, z: Double): EulerAngle = new EulerAngle(x, y, z)
}