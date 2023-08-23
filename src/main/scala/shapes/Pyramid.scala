/*
 *   Scala 3D renderer - pyramid shape case class
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

package dev.cptlobster.sc3d
package shapes

import core.Vertex

import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.mutable.ParArray

case class Pyramid(r: Double) extends Shape {
  override val points: ParArray[Vertex] = ParArray(
    Vertex(0, 1, 0), // 0
    Vertex(-1, -1, -1), // 1
    Vertex(-1, -1, 1), // 2
    Vertex(1, -1, -1), // 3
    Vertex(1, -1, 1) // 4
  ).map(_ * r)
  override val edges: ParSeq[(Int, Int)] = ParSeq(
    (0, 1),
    (0, 2),
    (0, 3),
    (0, 4),
    (1, 2),
    (1, 3),
    (2, 3),
    (3, 4)
  )
}