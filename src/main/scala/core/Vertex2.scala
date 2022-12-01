/*
 *   Scala 3D renderer - Basic 2-dimensional vertex
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
import scala.math.max

class Vertex2[T <: Double](val x: T, val y: T) extends Point[T] {
  def toArray: Array[T] = Array(x, y)
  override def toString: String = s"($x, $y)"
  def + (r : Vertex2[_]): Vertex2[T] = new Vertex2[T](x + r.x, y + r.y)
  def - (r : Vertex2[_]): Vertex2[T] = new Vertex2[T](x - r.x, y - r.y)
  def * (r : _ <: Double): Vertex2[T] = new Vertex2[T]((x * r).asInstanceOf[T], (y * r).asInstanceOf[T])
  def * (r : Vertex2[_]): Vertex2[T] = new Vertex2[T](x * r.x, y * r.y)
  def / (r : _ <: Double): Vertex2[T] = new Vertex2[T]((x / r).asInstanceOf[T], (y / r).asInstanceOf[T])
  def / (r : Vertex2[_]): Vertex2[T] = new Vertex2[T](x / r.x, y / r.y)
  def normalize: Vertex2[T] = this / max(x, y)
}

object Vertex2 {
  def apply[T <: Double](x: T, y: T): Vertex2[T] = { new Vertex2[T](x, y) }
  def up[T <: Double]: Vertex2[T] = new Vertex2(0.asInstanceOf[T], 1.asInstanceOf[T])
  def down[T <: Double]: Vertex2[T] = new Vertex2(0.asInstanceOf[T], (-1).asInstanceOf[T])
  def left[T <: Double]: Vertex2[T] = new Vertex2((-1).asInstanceOf[T], 0.asInstanceOf[T])
  def right[T <: Double]: Vertex2[T] = new Vertex2(1.asInstanceOf[T], 0.asInstanceOf[T])
  def identity[T <: Double]: Vertex2[T] = new Vertex2(0.asInstanceOf[T], 0.asInstanceOf[T])
  def zero[T <: Double]: Vertex2[T] = Vertex2.identity[T]

  implicit def fromVertex3Int[T <: Double](i: Vertex3Int[_]): Vertex2[T] = new Vertex2[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T])

  implicit def fromVertex3[T <: Double](i: Vertex3[_]): Vertex2[T] = new Vertex2[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T])

  implicit def fromVertex2Int[T <: Double](i: Vertex2Int[_]): Vertex2[T] = new Vertex2[T](i.x.asInstanceOf[T], i.y.asInstanceOf[T])
}
