/*
 *   Scala 3D renderer - main executor / entrypoint
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

import core.Vertex3
import shapes.Shape

import scala.math.Pi

object Main {
  def rotate_shape(shape: Shape, x: Double, y: Double, z: Double): Shape = {
    shape.rot += Vertex3(x, y, z)
    shape
  }

  /*
   * Make shapes spin.
   */
  def vectors: List[(Double, Double, Double)] = List(
    (0, Pi / 16, -Pi / 32),
    (-Pi / 16, Pi / 32, 0),
    (0, Pi / 64, 0)
  )

  def main(args: Array[String]): Unit = {
    val proj: Projector = new Projector()
    val rend: Renderer = new Renderer()
    print("\u001b[?25l")
    while (true) {
      rend.display(proj.projection)
      Thread.sleep(50)
      proj.shp.zip(vectors).map(p => rotate_shape(p._1, p._2._1, p._2._2, p._2._3))
    }
  }
}
