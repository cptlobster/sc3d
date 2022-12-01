/*
 *   Scala 3D renderer - Basic 3-dimensional vertex
 *   Copyright (C) 2022 Dustin Thomas
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

package org.cptlobster.sc3d
package core

import scala.math.max
import scala.language.implicitConversions

class Vertex3[T <: Double](val x: T, val y: T, val z: T) extends Point[T] {
  def toArray: Array[T] = Array(x, y, z)
  override def toString: String = s"($x, $y, $z)"
  /*
   * Arithmetic functions
   */
  def + (r : Vertex3[_]): Vertex3[T] = new Vertex3(x + r.x, y + r.y, z + r.z)
  def + (r : Vertex2[_]): Vertex3[T] = new Vertex3[T](x + r.x, y + r.y, z)

  def - (r : Vertex3[_]): Vertex3[T] = new Vertex3[T](x - r.x, y - r.y, z - r.z)
  def - (r : Vertex2[_]): Vertex3[T] = new Vertex3[T](x - r.x, y - r.y, z)
  def * (r : Vertex3[_]): Vertex3[T] = new Vertex3[T](x * r.x, y * r.y, z * r.z)
  def * (r : Vertex2[_]): Vertex3[T] = new Vertex3[T](x * r.x, y * r.y, z)
  def * (r : _ <: Double): Vertex3[T] = new Vertex3[T]((x * r).asInstanceOf[T], (y * r).asInstanceOf[T], (z * r).asInstanceOf[T])
  def / (r : Vertex3[_]): Vertex3[T] = new Vertex3[T](x / r.x, y / r.y, z / r.z)
  def / (r : Vertex2[_]): Vertex3[T] = new Vertex3[T](x / r.x, y / r.y, z)
  def / (r : _ <: Double): Vertex3[T] = new Vertex3[T]((x / r).asInstanceOf[T], (y / r).asInstanceOf[T], (z / r).asInstanceOf[T])
  def normalize: Vertex3[T] = this / max(max(x, y), z)
}

object Vertex3 {
  def apply[T <: Double](x: T, y: T, z: T): Vertex3[T] = new Vertex3(x, y, z)
  def up[T <: Double]: Vertex3[T] = new Vertex3(0.asInstanceOf[T], 1.asInstanceOf[T], 0.asInstanceOf[T])
  def down[T <: Double]: Vertex3[T] = new Vertex3(0.asInstanceOf[T], (-1).asInstanceOf[T], 0.asInstanceOf[T])
  def left[T <: Double]: Vertex3[T] = new Vertex3((-1).asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def right[T <: Double]: Vertex3[T] = new Vertex3(1.asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def forward[T <: Double]: Vertex3[T] = new Vertex3(0.asInstanceOf[T], 0.asInstanceOf[T], 1.asInstanceOf[T])
  def back[T <: Double]: Vertex3[T] = new Vertex3(0.asInstanceOf[T], 0.asInstanceOf[T], (-1).asInstanceOf[T])
  def identity[T <: Double]: Vertex3[T] = new Vertex3(0.asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def zero[T <: Double]: Vertex3[T] = Vertex3.identity[T]
  implicit def fromVertex3Int[T <: Double](i: Vertex3Int[_]): Vertex3[T] = new Vertex3[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T], i.z.asInstanceOf[T])
  implicit def fromVertex2[T <: Double](i: Vertex2[_]): Vertex3[T] = new Vertex3[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T], 0.asInstanceOf[T])
  implicit def fromVertex2Int[T <: Double](i: Vertex2Int[_]): Vertex3[T] = new Vertex3[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T], 0.asInstanceOf[T])
}