/*
 *   Scala 3D renderer - Trait of transformable object
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
package core

import core.Vertex
import scala.math.{cos, sin}

trait Transformable {
  var pos: Vertex = Vertex(0, 0, 0) // position
  var rot: Vertex = Vertex(0, 0, 0) // rotation
  var scl: Vertex = Vertex(1, 1, 1) // scale
  override def toString: String = s"P: $pos; R: $rot; S: $scl"
  def translate(direction: Vertex, magnitude: Double): Unit = pos += direction * magnitude
  def translate(direction: Vertex): Unit = pos += direction
  def rotate(angle: Vertex): Unit = rot += angle
  private def transformArr(a: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = a.length
    val cols = a.head.length
    val trans: Array[Array[Double]] = Array.ofDim(cols, rows)

    for (i <- 0 until cols; j <- 0 until rows) {
      trans(i)(j) = a(j)(i)
    }
    trans
  }
  private def matMult(a1: Vertex, a2: Array[Array[Double]]): Vertex = {
    val a2t = transformArr(a2)
    val j: Array[Double] = a1.toArray
    Vertex(
      j.zip(a2t(0)).map(a => a._1 * a._2).sum,
      j.zip(a2t(1)).map(a => a._1 * a._2).sum,
      j.zip(a2t(2)).map(a => a._1 * a._2).sum
    )
  }

  def rotMat(rx: Double, ry: Double, rz: Double): Array[Array[Double]] = Array(
    Array(cos(ry) * cos(rx), (sin(rz) * sin(ry) * cos(rx)) - (cos(rz) * sin(rx)), (cos(rz) * sin(ry) * cos(rx)) + (sin(rz) * sin(rx))),
    Array(cos(ry) * sin(rx), (sin(rz) * sin(ry) * sin(rx)) + (cos(rz) * cos(rx)), (cos(rz) * sin(ry) * sin(rx)) - (sin(rz) * cos(rx))),
    Array(-sin(ry), sin(rz) * cos(ry), cos(rz) * cos(ry))
  )

  def rotMat: Array[Array[Double]] = rotMat(this.rot.x, this.rot.y, this.rot.z)

  def rotateAround(position: Vertex, rotation: Vertex): Unit = {
    val distance: Vertex = this.pos - position
    this.pos - distance
    val rx = this.rot.x + rotation.x
    val ry = this.rot.y + rotation.y
    val rz = this.rot.z + rotation.z

    val rta: Array[Array[Double]] = rotMat(rx, ry, rz)
    val adjDistance = matMult(distance, rta)
    this.rot += rotation
    this.pos += adjDistance
  }
  def rotateAround(transformable: Transformable, rotation: Vertex): Unit = rotateAround(transformable.pos, rotation)
  def scale(scale: Vertex): Unit = scl += scale
}