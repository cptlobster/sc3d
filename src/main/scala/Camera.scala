/*
 *   Scala 3D renderer - camera object
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

import core.{Transformable, Vertex3}

case class Camera(p: Vertex3[Double] = Vertex3[Double](0, 0, 0),
                  r: Vertex3[Double] = Vertex3[Double](0, 0, 0),
                  var plane: Vertex3[Double] = Vertex3[Double](0, 0, 0)) extends Transformable {
  pos = p
  rot = r
}
