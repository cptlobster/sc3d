/*
 *   Scala 3D renderer - basic shape class and transformation functions
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

import core.{Transformable, Vertex}

import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.mutable.ParArray

abstract class Shape extends Transformable {
  val points: ParArray[Vertex] = ParArray()
  val edges: ParSeq[(Int, Int)] = ParSeq()

  private def transformArr(a: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = a.length
    val cols = a.head.length
    val trans: Array[Array[Double]] = Array.ofDim(cols, rows)

    for (i <- 0 until cols; j <- 0 until rows) {
      trans(i)(j) = a(j)(i)
    }
    trans
  }

  private def matMult(a1: Array[Vertex], a2: Array[Array[Double]]): Array[Vertex] = {
    val a2t = transformArr(a2)
    for (i <- a1) yield {
      val j: Array[Double] = i.toArray
      Vertex(
        j.zip(a2t(0)).map(a => a._1 * a._2).sum,
        j.zip(a2t(1)).map(a => a._1 * a._2).sum,
        j.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
  }

  private def matMult(a1: ParArray[Vertex], a2: Array[Array[Double]]): ParArray[Vertex] = {
    val a2t = transformArr(a2)
    for (i <- a1) yield {
      val j: Array[Double] = i.toArray
      Vertex(
        j.zip(a2t(0)).map(a => a._1 * a._2).sum,
        j.zip(a2t(1)).map(a => a._1 * a._2).sum,
        j.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
  }

  private def translate_pts(a: ParArray[Vertex], x: Double, y: Double, z: Double): ParArray[Vertex] = {
    a.map(v => Vertex(v.x + x, v.y + y, v.z + z))
  }

  private def rotate_pts(a: ParArray[Vertex], rx: Double, ry: Double, rz: Double): ParArray[Vertex] = {
    // https://en.wikipedia.org/wiki/Rotation_matrix#Basic_rotations
    val rta: Array[Array[Double]] = rotMat(rx, ry, rz)
    matMult(a, rta)
  }

  private def transform: ParArray[Vertex] = {
    val x = this.pos.x
    val y = this.pos.y
    val z = this.pos.z
    val rx = this.rot.x
    val ry = this.rot.y
    val rz = this.rot.z
    translate_pts(rotate_pts(this.points, rx, ry, rz), x, y, z)
  }

  def on_create(): Unit = {

  }

  def on_update(): Unit = {

  }

  def on_destroy(): Unit = {

  }
}