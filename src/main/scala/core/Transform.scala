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

import scala.annotation.tailrec
import scala.math.{cos, sin}

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
  override def toString: String = s"P: $position; R: $rotation; S: $scale"
  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object position, relative to its angle.
   *
   * @param pos The amount to change position by.
   * @return A transformation matrix with the updated position.
   */
  def translate(vector: Vertex): Transform = translate_global(vector.rotate(rotation))

  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object position, relative to world space.
   *
   * @param pos The amount to change position by.
   * @return A transformation matrix with the updated position.
   */
  def translate_global(vector: Vertex): Transform = Transform(position + vector, rotation, scale)
  /** Add a [[dev.cptlobster.sc3d.core.Vertex]] to the current object rotation.
   *
   * @param angle The amount to change rotation by.
   * @return A transformation matrix with the updated rotation.
   */
  def rotate(angle: EulerAngle): Transform = Transform(position, rotation + angle, scale)

  private def transform_array(a: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = a.length
    val cols = a.head.length
    val trans: Array[Array[Double]] = Array.ofDim(cols, rows)

    for (i <- 0 until cols; j <- 0 until rows) {
      trans(i)(j) = a(j)(i)
    }
    trans
  }

  private def matrix_multiply(a1: Vertex, a2: Array[Array[Double]]): Vertex = {
    val a2t = transform_array(a2)
    val j: Array[Double] = a1.toArray
    Vertex(
      j.zip(a2t(0)).map(a => a._1 * a._2).sum,
      j.zip(a2t(1)).map(a => a._1 * a._2).sum,
      j.zip(a2t(2)).map(a => a._1 * a._2).sum
    )
  }

  def rot_mat(rx: Double, ry: Double, rz: Double): Array[Array[Double]] = Array(
    Array(cos(ry) * cos(rx), (sin(rz) * sin(ry) * cos(rx)) - (cos(rz) * sin(rx)), (cos(rz) * sin(ry) * cos(rx)) + (sin(rz) * sin(rx))),
    Array(cos(ry) * sin(rx), (sin(rz) * sin(ry) * sin(rx)) + (cos(rz) * cos(rx)), (cos(rz) * sin(ry) * sin(rx)) - (sin(rz) * cos(rx))),
    Array(-sin(ry), sin(rz) * cos(ry), cos(rz) * cos(ry))
  )

  def rot_mat: Array[Array[Double]] = rot_mat(rotation.x, rotation.y, rotation.z)

  /** Change the object's angle around a pivot point that is not the center of the object.
   *
   * @param position The pivot [[dev.cptlobster.sc3d.core.Vertex]].
   * @param rotation The amount to change rotation by.
   * @return A transformation matrix with the updated position.
   */
  def rotate_around(pivot: Vertex, angle: EulerAngle): Transform = {
    val translation: Vertex = position - pivot
    val rx = rotation.x + angle.x
    val ry = rotation.y + angle.y
    val rz = rotation.z + angle.z

    val rta: Array[Array[Double]] = rot_mat(rx, ry, rz)
    val new_vertex = matrix_multiply(translation, rta)
    Transform(pivot + new_vertex, rotation + angle, scale)
  }

  /** Change the object's angle around a pivot point that is not the center of the object.
   *
   * @param transform The [[Transform]] whose position we will use to pivot
   * @param rotation The amount to change rotation by.
   * @return A transformation matrix with the updated position.
   */
  def rotate_around(transform: Transform, angle: EulerAngle): Transform = rotate_around(transform.position, angle)

  def scale(magnitude: Vertex): Transform = Transform(position, rotation, scale + magnitude)
}