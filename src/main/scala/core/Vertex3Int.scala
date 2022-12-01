/*
 *   Scala 3D renderer - Basic 3-dimensional vertex (integer)
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

import scala.language.implicitConversions
import scala.math.{max, round}

class Vertex3Int[T <: Int](val x: T, val y: T, val z: T) extends Point[T] {
  def toArray: Array[T] = Array(x, y, z)
  override def toString: String = s"($x, $y, $z)"
  /*
   * Arithmetic functions
   */
  def + (r : Vertex3Int[_]): Vertex3Int[T] = new Vertex3Int(x + r.x, y + r.y, z + r.z)
  def + (r : Vertex2Int[_]): Vertex3Int[T] = new Vertex3Int[T](x + r.x, y + r.y, z)

  def - (r : Vertex3Int[_]): Vertex3Int[T] = new Vertex3Int[T](x - r.x, y - r.y, z - r.z)
  def - (r : Vertex2Int[_]): Vertex3Int[T] = new Vertex3Int[T](x - r.x, y - r.y, z)
  def * (r : Vertex3Int[_]): Vertex3Int[T] = new Vertex3Int[T](x * r.x, y * r.y, z * r.z)
  def * (r : Vertex2Int[_]): Vertex3Int[T] = new Vertex3Int[T](x * r.x, y * r.y, z)
  def * (r : _ <: Int): Vertex3Int[T] = new Vertex3Int[T]((x * r).asInstanceOf[T], (y * r).asInstanceOf[T], (z * r).asInstanceOf[T])
  def / (r : Vertex3Int[_]): Vertex3Int[T] = new Vertex3Int[T](x / r.x, y / r.y, z / r.z)
  def / (r : Vertex2Int[_]): Vertex3Int[T] = new Vertex3Int[T](x / r.x, y / r.y, z)
  def / (r : _ <: Int): Vertex3Int[T] = new Vertex3Int[T]((x / r).asInstanceOf[T], (y / r).asInstanceOf[T], (z / r).asInstanceOf[T])
  def normalize: Vertex3Int[T] = this / max(max(x, y), z)
}

object Vertex3Int {
  def apply[T <: Int](x: T, y: T, z: T): Vertex3Int[T] = new Vertex3Int(x, y, z)
  def up[T <: Int]: Vertex3Int[T] = new Vertex3Int(0.asInstanceOf[T], 1.asInstanceOf[T], 0.asInstanceOf[T])
  def down[T <: Int]: Vertex3Int[T] = new Vertex3Int(0.asInstanceOf[T], (-1).asInstanceOf[T], 0.asInstanceOf[T])
  def left[T <: Int]: Vertex3Int[T] = new Vertex3Int((-1).asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def right[T <: Int]: Vertex3Int[T] = new Vertex3Int(1.asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def forward[T <: Int]: Vertex3Int[T] = new Vertex3Int(0.asInstanceOf[T], 0.asInstanceOf[T], 1.asInstanceOf[T])
  def back[T <: Int]: Vertex3Int[T] = new Vertex3Int(0.asInstanceOf[T], 0.asInstanceOf[T], (-1).asInstanceOf[T])
  def identity[T <: Int]: Vertex3Int[T] = new Vertex3Int(0.asInstanceOf[T], 0.asInstanceOf[T], 0.asInstanceOf[T])
  def zero[T <: Int]: Vertex3Int[T] = Vertex3Int.identity[T]
  implicit def fromVertex3[T <: Int](i: Vertex3[_]): Vertex2Int[T] = new Vertex2Int[T](round[T](i.x), round[T](i.y))
  implicit def fromVertex2[T <: Int](i: Vertex2[_]): Vertex2Int[T] = new Vertex2Int[T](round[T](i.x), round[T](i.y))
  implicit def fromVertex3Int[T <: Int](i: Vertex3Int[_]): Vertex2Int[T] = new Vertex2Int[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T])
}