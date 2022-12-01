/*
 *   Scala 3D renderer - sphere shape case class
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
package shapes

import core.Vertex3

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.mutable.ParArray
import scala.math.{Pi, cos, sin}

case class Sphere(r: Double, p: Int) extends Shape {
  override val points: ParArray[Vertex3[Double]] = (for (i <- 0 until 2 * p; j <- 0 until p) yield {
    val a1: Double = i * Pi / p
    val a2: Double = j * Pi / p
    Vertex3[Double](sin(a1) * cos(a2), sin(a1) * sin(a2), cos(a1)) * r
  }).toArray.par

  override val edges: ParSeq[(Int, Int)] = ParSeq()
}