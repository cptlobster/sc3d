/*
 *   Scala 3D renderer - Transform class
 *   Copyright (C) 2022-2023 Dustin Thomas
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
package core

/** A class for handling object positioning in 3D space.
 * == Example ==
 * {{{
 *   val initial: Transform = Transform(
 *     Vertex(0, 0, 0),
 *     Vertex(0, 0, 0),
 *     Vertex(1, 1, 1)
 *   ) // creates an object at world center, facing north.
 *   val final: Transform = initial.translate(Vertex.forward) # Move 1m north.
 * }}}
 *
 * @param position The position of the object in 3D space.
 * @param rotation The forward-facing angle of the object.
 * @param scale The size of the object.
 */
case class Transform(position: Vertex, rotation: EulerAngle, scale: Vertex) {
  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object position, relative to its angle.
   *
   * @param pos The amount to change position by.
   * @return A transformation matrix with the updated position.
   */
  def translate(pos: Vertex): Transform = this.translate_global(pos.rotate(rotation))

  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object position, relative to world space.
   *
   * @param pos The amount to change position by.
   * @return A transformation matrix with the updated position.
   */
  def translate_global(pos: Vertex): Transform = Transform(position + pos, rotation, scale)
  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object rotation.
   *
   * @param angle The amount to change rotation by.
   * @return A transformation matrix with the updated rotation.
   */
  def rotate(angle: EulerAngle): Transform = Transform(position, rotation + angle, scale)

  /** Change the object's angle around a pivot point that is not the center of the object.
   *
   * @param pos The pivot [[dev.cptlobster.sc3d.core.Vertex]].
   * @param angle The amount to change rotation by.
   * @return A transformation matrix with the updated position.
   */
  def rotate_around(pos: Vertex, angle: EulerAngle): Transform = {
    val magnitude = position.distance_from(pos)
    ???
  }
}