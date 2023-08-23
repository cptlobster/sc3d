/*
 *   Scala 3D renderer - pyramid shape case class
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

class Vertex(val x: Double, val y: Double, val z: Double) extends Point {
  // def as[A]: Vertex[A] = new Vertex[A](this.x.asInstanceOf[A], this.y.asInstanceOf[A], this.z.asInstanceOf[A])
  def toArray: Array[Double] = Array(x, y, z)
  def toVector: Vector[Double] = Vector(x, y, z)
  override def toString: String = s"($x, $y, $z)"
  def normalize: Vertex = this / max(max(x, y), z)

  /*
   * Arithmetic functions
   */
  def +(r: Vertex): Vertex = new Vertex(x + r.x, y + r.y, z + r.z)
  //def +(r: Vertex[_]): Vertex = this + r.as[Double]
  def -(r: Vertex): Vertex = new Vertex(x - r.x, y - r.y, z - r.z)
  //def -(r: Vertex[_]): Vertex = this - r.as[Double]
  def *(r: Vertex): Vertex = new Vertex(x * r.x, y * r.y, z * r.z)
  //def *(r: Vertex[_]): Vertex = this * r.as[Double]
  def *(r: Double): Vertex = this * new Vertex(r, r, r)
  def /(r: Vertex): Vertex = new Vertex(x / r.x, y / r.y, z / r.z)
  //def /(r: Vertex[_]): Vertex = this / r.as[Double]
  def /(r: Double): Vertex = this / new Vertex(r, r, r)

  def distance_from(point: Vertex): Double = {
    val p1: List[Double] = List(this.x, this.y, this.z)
    val p2: List[Double] = List(point.x, point.y, point.z)
    val partials: List[Double] = p1.zip(p2).map(x => x._1 + x._2)
    sqrt(partials.map(x => pow(x, 2)).sum)
  }
}

object Vertex {
  def apply(x: Double, y: Double, z: Double): Vertex = new Vertex(x, y, z)

  def apply(x: Double, y: Double): Vertex = new Vertex(x, y, 0.asInstanceOf[Double])

  def up: Vertex = new Vertex(0.asInstanceOf[Double], 1.asInstanceOf[Double], 0.asInstanceOf[Double])

  def down: Vertex = new Vertex(0.asInstanceOf[Double], (-1).asInstanceOf[Double], 0.asInstanceOf[Double])

  def left: Vertex = new Vertex((-1).asInstanceOf[Double], 0.asInstanceOf[Double], 0.asInstanceOf[Double])

  def right: Vertex = new Vertex(1.asInstanceOf[Double], 0.asInstanceOf[Double], 0.asInstanceOf[Double])

  def forward: Vertex = new Vertex(0.asInstanceOf[Double], 0.asInstanceOf[Double], 1.asInstanceOf[Double])

  def back: Vertex = new Vertex(0.asInstanceOf[Double], 0.asInstanceOf[Double], (-1).asInstanceOf[Double])

  def identity: Vertex = new Vertex(0.asInstanceOf[Double], 0.asInstanceOf[Double], 0.asInstanceOf[Double])

  def zero: Vertex = Vertex.identity
}