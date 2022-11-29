/*
 *   Scala 3D renderer - Base vertex datatype
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

package org.cptlobster

case class Vertex(val x: Double, val y: Double, val z: Double) {
  def asArray: Array[Double] = Array(x, y, z)
  override def toString: String = s"($x, $y, $z)"
  def + (r : Vertex): Vertex = new Vertex(x + r.x, y + r.y, z + r.z)
  def - (r : Vertex): Vertex = new Vertex(x - r.x, y - r.y, z - r.z)
  def * (r : Double): Vertex = new Vertex(x * r, y * r, z * r)
  def * (r : Vertex): Vertex = new Vertex(x * r.x, y * r.y, z * r.z)
  def / (r : Double): Vertex = new Vertex(x / r, y / r, z / r)
  def / (r : Vertex): Vertex = new Vertex(x / r.x, y / r.y, z / r.z)
}
