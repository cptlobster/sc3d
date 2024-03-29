/*
 *   Scala 3D renderer - Vertex case class
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

import scala.math.{max, pow, sqrt}

/** Basic class for performing vector math in a 3-dimensional space.
 *
 * @note All instances use [[scala.Double]]s to store values.
 *
 * @constructor Create a new Vertex point.
 * @param x Location on the x-axis
 * @param y Location on the y-axis
 * @param z Location on the z-axis
 */
class Vertex(val x: Double, val y: Double, val z: Double) {
  /** Return this [[Vertex]] as an [[scala.Array]] of [[scala.Double]]s. */
  def toArray: Array[Double] = Array(x, y, z)
  /** Return this [[Vertex]] as an [[scala.collection.immutable.Vector]] of [[scala.Double]]s. */
  def toVector: Vector[Double] = Vector(x, y, z)
  /** Return this [[Vertex]] as a [[String]], in format `"(x, y, z)"`. */
  override def toString: String = s"($x, $y, $z)"
  /** Normalize this [[Vertex]] to a length of 1. This is done by dividing by the largest
   * coordinate value.
   */
  def normalize: Vertex = this / max(max(x, y), z)
  def +(r: Vertex): Vertex = new Vertex(x + r.x, y + r.y, z + r.z)
  def -(r: Vertex): Vertex = new Vertex(x - r.x, y - r.y, z - r.z)
  def *(r: Vertex): Vertex = new Vertex(x * r.x, y * r.y, z * r.z)
  def *(r: Double): Vertex = this * new Vertex(r, r, r)
  def /(r: Vertex): Vertex = new Vertex(x / r.x, y / r.y, z / r.z)
  def /(r: Double): Vertex = this / new Vertex(r, r, r)

  /** Get the distance between two [[Vertex]]es.
   *
   * @param point The second point to get distance between
   * @return The distance between the two points, as a [[scala.Double]]*/
  def distance_from(point: Vertex): Double = {
    val p1: List[Double] = List(this.x, this.y, this.z)
    val p2: List[Double] = List(point.x, point.y, point.z)
    val partials: List[Double] = p1.zip(p2).map(x => x._1 + x._2)
    sqrt(partials.map(x => pow(x, 2)).sum)
  }
  def magnitude: Double = distance_from(new Vertex(0, 0, 0))
  /** Rotate this [[Vertex]]'s direction by the specified angle.
   *
   * @param angle The angle to rotate this vector's direction by
   * @return The vertex, rotated by the specified angle.
   **/
  def rotate(angle: EulerAngle): Vertex = ???
  def rotate_around(angle: EulerAngle, pivot: Vertex): Vertex = ???
}

object Vertex {
  def apply(x: Double, y: Double, z: Double): Vertex = new Vertex(x, y, z)

  /** Create a [[Vertex]] pointing directly up (positive Y). */
  def up: Vertex = new Vertex(0d, 1d, 0d)
  /** Create a [[Vertex]] pointing directly down (negative Y). */
  def down: Vertex = new Vertex(0d, -1d, 0d)
  /** Create a [[Vertex]] pointing directly left (negative X). */
  def left: Vertex = new Vertex((-1)d, 0d, 0d)
  /** Create a [[Vertex]] pointing directly right (positive X). */
  def right: Vertex = new Vertex(1d, 0d, 0d)
  /** Create a [[Vertex]] pointing directly forward (positive Z). */
  def forward: Vertex = new Vertex(0d, 0d, 1d)
  /** Create a [[Vertex]] pointing directly backward (negative Z). */
  def back: Vertex = new Vertex(0d, 0d, -1d)
  /** Create a [[Vertex]] with a value of zero. */
  def identity: Vertex = new Vertex(0d, 0d, 0d)
  /** Create a [[Vertex]] with a value of zero. */
  def zero: Vertex = Vertex.identity
}