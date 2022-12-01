/*
 *   Scala 3D renderer - Basic 2-dimensional vertex (integer)
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

class Vertex2Int[T <: Int](val x: T, val y: T) extends Point[T] {
  def toArray: Array[T] = Array(x, y)
  override def toString: String = s"($x, $y)"
  def + (r : Vertex2Int[_]): Vertex2Int[T] = new Vertex2Int[T](x + r.x, y + r.y)
  def - (r : Vertex2Int[_]): Vertex2Int[T] = new Vertex2Int[T](x - r.x, y - r.y)
  def * (r : _ <: Int): Vertex2Int[T] = new Vertex2Int[T]((x * r).asInstanceOf[T], (y * r).asInstanceOf[T])
  def * (r : Vertex2Int[_]): Vertex2Int[T] = new Vertex2Int[T](x * r.x, y * r.y)
  def / (r : _ <: Int): Vertex2Int[T] = new Vertex2Int[T]((x / r).asInstanceOf[T], (y / r).asInstanceOf[T])
  def / (r : Vertex2Int[_]): Vertex2Int[T] = new Vertex2Int[T](x / r.x, y / r.y)
  def normalize: Vertex2Int[T] = this / max(x, y)
}

object Vertex2Int {
  def apply[T <: Int](x: T, y: T): Vertex2Int[T] = { new Vertex2Int[T](x, y) }
  def up[T <: Int]: Vertex2Int[T] = new Vertex2Int(0.asInstanceOf[T], 1.asInstanceOf[T])
  def down[T <: Int]: Vertex2Int[T] = new Vertex2Int(0.asInstanceOf[T], (-1).asInstanceOf[T])
  def left[T <: Int]: Vertex2Int[T] = new Vertex2Int((-1).asInstanceOf[T], 0.asInstanceOf[T])
  def right[T <: Int]: Vertex2Int[T] = new Vertex2Int(1.asInstanceOf[T], 0.asInstanceOf[T])
  def identity[T <: Int]: Vertex2Int[T] = new Vertex2Int(0.asInstanceOf[T], 0.asInstanceOf[T])
  def zero[T <: Int]: Vertex2Int[T] = Vertex2Int.identity[T]
  implicit def fromVertex3Int[T <: Int](i: Vertex3Int[_]): Vertex2Int[T] = new Vertex2Int[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T])
  implicit def fromVertex3[T <: Int](i: Vertex3[_]): Vertex2Int[T] = new Vertex2Int[T](round[T](i.x), round[T](i.y))
  implicit def fromVertex2[T <: Int](i: Vertex2[_]): Vertex2Int[T] = new Vertex2Int[T](round[T](i.x), round[T](i.y))
}
