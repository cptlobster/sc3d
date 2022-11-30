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

import scala.collection.parallel.immutable.ParSeq

class Projector {
  /*
   * This is where you add shapes. Simply append them to the list. List of available shapes in `Shape.scala`
   */
  var shp: ParSeq[Shape] = ParSeq(Cube(4), Cube(2), Pyramid(1))

  var cam: Camera = Camera()
  /*
   * If you so desire, you can move the camera with the following:
   */
  cam.pos += Vertex(0, 0, 15)
  cam.rot += Vertex(0, 0, 0)
  cam.plane += Vertex(0, 0, 8)

  /*
   * Almost everything past here is hell. Enter at your own risk.
   * If you want to control how shapes rotate, jump down to `main.vectors` and edit items in that list.
   */

  def projection: ParSeq[ProjectedShape] = {
    shp.map(_.project(cam))
  }
}