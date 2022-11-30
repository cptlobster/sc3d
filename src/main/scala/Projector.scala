/*
 *   Scala 3D renderer - shape projector from 3D -> 2D
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

import scala.math.Pi

class Projector {
  /*
   * This is where you add shapes. Simply append them to the list. List of available shapes in `Shape.scala`
   */
  // var shp: List[Shape] = List(Cube(4), Cube(2), Pyramid(1))
  var shp: List[Shape] = List(
    Poly(
      Array(
        Vertex(2, 0, 0),
        Vertex(-2, 0, 0)
      ),
      List((0, 1))
    )
  )

  var cam: Camera = Camera()
  /*
   * If you so desire, you can move the camera with the following:
   */
  cam.pos += new Vertex(0, 0, 15)
  cam.rot += new Vertex(0, 0, 0)
  cam.plane += new Vertex(0, 0, 8)

  /*
   * Almost everything past here is hell. Enter at your own risk.
   * If you want to control how shapes rotate, jump down to `main.vectors` and edit items in that list.
   */

  def projection: List[ProjectedShape] = {
    shp.map(_.project(cam))
  }
}