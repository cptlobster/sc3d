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

package org.cptlobster.sc3d
package shapes

import core.{Transformable, Vertex2, Vertex3}

import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.mutable.ParArray

abstract class Shape extends Transformable {
  val points: ParArray[Vertex3[Double]] = ParArray()
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

  private def matMult(a1: Array[Vertex3[Double]], a2: Array[Array[Double]]): Array[Vertex3[Double]] = {
    val a2t = transformArr(a2)
    for (i <- a1) yield {
      val j: Array[Double] = i.toArray
      Vertex3(
        j.zip(a2t(0)).map(a => a._1 * a._2).sum,
        j.zip(a2t(1)).map(a => a._1 * a._2).sum,
        j.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
  }

  private def matMult(a1: ParArray[Vertex3[Double]], a2: Array[Array[Double]]): ParArray[Vertex3[Double]] = {
    val a2t = transformArr(a2)
    for (i <- a1) yield {
      val j: Array[Double] = i.toArray
      Vertex3(
        j.zip(a2t(0)).map(a => a._1 * a._2).sum,
        j.zip(a2t(1)).map(a => a._1 * a._2).sum,
        j.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
  }

  private def translate_pts(a: ParArray[Vertex3[Double]], x: Double, y: Double, z: Double): ParArray[Vertex3[Double]] = {
    a.map(v => Vertex3(v.x + x, v.y + y, v.z + z))
  }

  private def rotate_pts(a: ParArray[Vertex3[Double]], rx: Double, ry: Double, rz: Double): ParArray[Vertex3[Double]] = {
    // https://en.wikipedia.org/wiki/Rotation_matrix#Basic_rotations
    val rta: Array[Array[Double]] = rotMat(rx, ry, rz)
    matMult(a, rta)
  }

  private def transform: ParArray[Vertex3[Double]] = {
    val x = this.pos.x
    val y = this.pos.y
    val z = this.pos.z
    val rx = this.rot.x
    val ry = this.rot.y
    val rz = this.rot.z
    translate_pts(rotate_pts(this.points, rx, ry, rz), x, y, z)
  }

  def project(c: Camera): ProjectedShape = {
    val ex = c.plane.x
    val ey = c.plane.y
    val ez = c.plane.z
    val adj_pts: ParArray[Vertex3[Double]] = translate_pts(transform, -c.pos.x, -c.pos.y, -c.pos.z)

    ProjectedShape(adj_pts.map(a => Vertex2[Double](((ez / a.z) * a.x) + ex, ((ez / a.z) * a.y) + ey)), edges)
  }
}