/*
 *   Scala 3D renderer - basic shape and transformation functions
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

import scala.math.{Pi, cos, sin}

abstract class Shape extends Transformable {
  val points: Array[Vertex]
  val edges: List[(Int, Int)]

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
      Vertex(
        i.asArray.zip(a2t(0)).map(a => a._1 * a._2).sum,
        i.asArray.zip(a2t(1)).map(a => a._1 * a._2).sum,
        i.asArray.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
  }

  def translate(a: Array[Vertex], x: Double, y: Double, z: Double): Array[Vertex] = {
    a.map(v => Vertex(v.x + x, v.y + y, v.z + z))
  }

  def rotate(a: Array[Vertex], rx: Double, ry: Double, rz: Double): Array[Vertex] = {
    // https://en.wikipedia.org/wiki/Rotation_matrix#Basic_rotations
    val rta: Array[Array[Double]] = Array(
      Array(cos(ry) * cos(rx), (sin(rz) * sin(ry) * cos(rx)) - (cos(rz) * sin(rx)), (cos(rz) * sin(ry) * cos(rx)) + (sin(rz) * sin(rx))),
      Array(cos(ry) * sin(rx), (sin(rz) * sin(ry) * sin(rx)) + (cos(rz) * cos(rx)), (cos(rz) * sin(ry) * sin(rx)) - (sin(rz) * cos(rx))),
      Array(-sin(ry), sin(rz) * cos(ry), cos(rz) * cos(ry))
    )
    matMult(a, rta)
  }

  def transform: Array[Vertex] = {
    val x = this.pos.x
    val y = this.pos.y
    val z = this.pos.z
    val rx = this.rot.x
    val ry = this.rot.y
    val rz = this.rot.z
    translate(rotate(this.points, rx, ry, rz), x, y, z)
  }

  def project(c: Camera): ProjectedShape = {
    val ex = c.plane.x
    val ey = c.plane.y
    val ez = c.plane.z
    val adj_pts: Array[Vertex] = translate(transform, -c.pos.x, -c.pos.y, -c.pos.z)

    ProjectedShape(adj_pts.map(a => Array(((ez / a.z) * a.x) + ex, ((ez / a.z) * a.y) + ey)), edges)
  }
}

case class Poly(points: Array[Vertex], edges: List[(Int, Int)]) extends Shape {

}

case class Cube(r: Double) extends Shape {
  override val points: Array[Vertex] = Array(
    Vertex(1, 1, 1), // 0, A
    Vertex(-1, 1, 1), // 1, B
    Vertex(1, -1, 1), // 2, C
    Vertex(1, 1, -1), // 3, D
    Vertex(1, -1, -1), // 4, E
    Vertex(-1, 1, -1), // 5, F
    Vertex(-1, -1, 1), // 6, G
    Vertex(-1, -1, -1) // 7, H
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List(
    (0, 1),
    (0, 2),
    (0, 3),
    (1, 5),
    (1, 6),
    (2, 4),
    (2, 6),
    (3, 4),
    (3, 5),
    (4, 7),
    (5, 7),
    (6, 7)
  )
}

case class Pyramid(r: Double) extends Shape {
  override val points: Array[Vertex] = Array(
    Vertex(0, 1, 0), // 0
    Vertex(-1, -1, -1), // 1
    Vertex(-1, -1, 1), // 2
    Vertex(1, -1, -1), // 3
    Vertex(1, -1, 1) // 4
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List(
    (0, 1),
    (0, 2),
    (0, 3),
    (0, 4),
    (1, 2),
    (1, 3),
    (2, 3),
    (3, 4)
  )
}

case class Sphere(r: Double, p: Int) extends Shape {
  override val points: Array[Vertex] = (for (i <- 0 until 2 * p; j <- 0 until p) yield {
    val a1: Double = i * Pi / p
    val a2: Double = j * Pi / p
    Vertex(sin(a1) * cos(a2), sin(a1) * sin(a2), cos(a1)) * r
  }).toArray

  override val edges: List[(Int, Int)] = List()
}

case class Utah(r: Double) extends Shape {
  rot += Vertex(-Pi / 2, 0, 0)

  override val points: Array[Vertex] = Array(
    Vertex(1.40000, 0.00000, 2.40000),
    Vertex(1.40000, -0.78400, 2.40000),
    Vertex(0.78000, -1.40000, 2.40000),
    Vertex(0.00000, -1.40000, 2.40000),
    Vertex(1.33750, 0.00000, 2.53125),
    Vertex(1.33750, -0.74900, 2.53125),
    Vertex(0.74900, -1.33750, 2.53125),
    Vertex(0.00000, -1.33750, 2.53125),
    Vertex(1.43750, 0.00000, 2.53125),
    Vertex(1.43750, -0.80500, 2.53125),
    Vertex(0.80500, -1.43750, 2.53125),
    Vertex(0.00000, -1.43750, 2.53125),
    Vertex(1.50000, 0.00000, 2.40000),
    Vertex(1.50000, -0.84000, 2.40000),
    Vertex(0.84000, -1.50000, 2.40000),
    Vertex(0.00000, -1.50000, 2.40000),
    Vertex(-0.78400, -1.40000, 2.40000),
    Vertex(-1.40000, -0.78400, 2.40000),
    Vertex(-1.40000, 0.00000, 2.40000),
    Vertex(-0.74900, -1.33750, 2.53125),
    Vertex(-1.33750, -0.74900, 2.53125),
    Vertex(-1.33750, 0.00000, 2.53125),
    Vertex(-0.80500, -1.43750, 2.53125),
    Vertex(-1.43750, -0.80500, 2.53125),
    Vertex(-1.43750, 0.00000, 2.53125),
    Vertex(-0.84000, -1.50000, 2.40000),
    Vertex(-1.50000, -0.84000, 2.40000),
    Vertex(-1.50000, 0.00000, 2.40000),
    Vertex(-1.40000, 0.78400, 2.40000),
    Vertex(-0.78400, 1.40000, 2.40000),
    Vertex(0.00000, 1.40000, 2.40000),
    Vertex(-1.33750, 0.74900, 2.53125),
    Vertex(-0.74900, 1.33750, 2.53125),
    Vertex(0.00000, 1.33750, 2.53125),
    Vertex(-1.43750, 0.80500, 2.53125),
    Vertex(-0.80500, 1.43750, 2.53125),
    Vertex(0.00000, 1.43750, 2.53125),
    Vertex(-1.50000, 0.84000, 2.40000),
    Vertex(-0.84000, 1.50000, 2.40000),
    Vertex(0.00000, 1.50000, 2.40000),
    Vertex(0.78400, 1.40000, 2.40000),
    Vertex(1.40000, 0.78400, 2.40000),
    Vertex(0.74900, 1.33750, 2.53125),
    Vertex(1.33750, 0.74900, 2.53125),
    Vertex(0.80500, 1.43750, 2.53125),
    Vertex(1.43750, 0.80500, 2.53125),
    Vertex(0.84000, 1.50000, 2.40000),
    Vertex(1.50000, 0.84000, 2.40000),
    Vertex(1.75000, 0.00000, 1.87500),
    Vertex(1.75000, -0.98000, 1.87500),
    Vertex(0.98000, -1.75000, 1.87500),
    Vertex(0.00000, -1.75000, 1.87500),
    Vertex(2.00000, 0.00000, 1.35000),
    Vertex(2.00000, -1.12000, 1.35000),
    Vertex(1.12000, -2.00000, 1.35000),
    Vertex(0.00000, -2.00000, 1.35000),
    Vertex(2.00000, 0.00000, 0.90000),
    Vertex(2.00000, -1.12000, 0.90000),
    Vertex(1.12000, -2.00000, 0.90000),
    Vertex(0.00000, -2.00000, 0.90000),
    Vertex(-0.98000, -1.75000, 1.87500),
    Vertex(-1.75000, -0.98000, 1.87500),
    Vertex(-1.75000, 0.00000, 1.87500),
    Vertex(-1.12000, -2.00000, 1.35000),
    Vertex(-2.00000, -1.12000, 1.35000),
    Vertex(-2.00000, 0.00000, 1.35000),
    Vertex(-1.12000, -2.00000, 0.90000),
    Vertex(-2.00000, -1.12000, 0.90000),
    Vertex(-2.00000, 0.00000, 0.90000),
    Vertex(-1.75000, 0.98000, 1.87500),
    Vertex(-0.98000, 1.75000, 1.87500),
    Vertex(0.00000, 1.75000, 1.87500),
    Vertex(-2.00000, 1.12000, 1.35000),
    Vertex(-1.12000, 2.00000, 1.35000),
    Vertex(0.00000, 2.00000, 1.35000),
    Vertex(-2.00000, 1.12000, 0.90000),
    Vertex(-1.12000, 2.00000, 0.90000),
    Vertex(0.00000, 2.00000, 0.90000),
    Vertex(0.98000, 1.75000, 1.87500),
    Vertex(1.75000, 0.98000, 1.87500),
    Vertex(1.12000, 2.00000, 1.35000),
    Vertex(2.00000, 1.12000, 1.35000),
    Vertex(1.12000, 2.00000, 0.90000),
    Vertex(2.00000, 1.12000, 0.90000),
    Vertex(2.00000, 0.00000, 0.45000),
    Vertex(2.00000, -1.12000, 0.45000),
    Vertex(1.12000, -2.00000, 0.45000),
    Vertex(0.00000, -2.00000, 0.45000),
    Vertex(1.50000, 0.00000, 0.22500),
    Vertex(1.50000, -0.84000, 0.22500),
    Vertex(0.84000, -1.50000, 0.22500),
    Vertex(0.00000, -1.50000, 0.22500),
    Vertex(1.50000, 0.00000, 0.15000),
    Vertex(1.50000, -0.84000, 0.15000),
    Vertex(0.84000, -1.50000, 0.15000),
    Vertex(0.00000, -1.50000, 0.15000),
    Vertex(-1.12000, -2.00000, 0.45000),
    Vertex(-2.00000, -1.12000, 0.45000),
    Vertex(-2.00000, 0.00000, 0.45000),
    Vertex(-0.84000, -1.50000, 0.22500),
    Vertex(-1.50000, -0.84000, 0.22500),
    Vertex(-1.50000, 0.00000, 0.22500),
    Vertex(-0.84000, -1.50000, 0.15000),
    Vertex(-1.50000, -0.84000, 0.15000),
    Vertex(-1.50000, 0.00000, 0.15000),
    Vertex(-2.00000, 1.12000, 0.45000),
    Vertex(-1.12000, 2.00000, 0.45000),
    Vertex(0.00000, 2.00000, 0.45000),
    Vertex(-1.50000, 0.84000, 0.22500),
    Vertex(-0.84000, 1.50000, 0.22500),
    Vertex(0.00000, 1.50000, 0.22500),
    Vertex(-1.50000, 0.84000, 0.15000),
    Vertex(-0.84000, 1.50000, 0.15000),
    Vertex(0.00000, 1.50000, 0.15000),
    Vertex(1.12000, 2.00000, 0.45000),
    Vertex(2.00000, 1.12000, 0.45000),
    Vertex(0.84000, 1.50000, 0.22500),
    Vertex(1.50000, 0.84000, 0.22500),
    Vertex(0.84000, 1.50000, 0.15000),
    Vertex(1.50000, 0.84000, 0.15000),
    Vertex(-1.60000, 0.00000, 2.02500),
    Vertex(-1.60000, -0.30000, 2.02500),
    Vertex(-1.50000, -0.30000, 2.25000),
    Vertex(-1.50000, 0.00000, 2.25000),
    Vertex(-2.30000, 0.00000, 2.02500),
    Vertex(-2.30000, -0.30000, 2.02500),
    Vertex(-2.50000, -0.30000, 2.25000),
    Vertex(-2.50000, 0.00000, 2.25000),
    Vertex(-2.70000, 0.00000, 2.02500),
    Vertex(-2.70000, -0.30000, 2.02500),
    Vertex(-3.00000, -0.30000, 2.25000),
    Vertex(-3.00000, 0.00000, 2.25000),
    Vertex(-2.70000, 0.00000, 1.80000),
    Vertex(-2.70000, -0.30000, 1.80000),
    Vertex(-3.00000, -0.30000, 1.80000),
    Vertex(-3.00000, 0.00000, 1.80000),
    Vertex(-1.50000, 0.30000, 2.25000),
    Vertex(-1.60000, 0.30000, 2.02500),
    Vertex(-2.50000, 0.30000, 2.25000),
    Vertex(-2.30000, 0.30000, 2.02500),
    Vertex(-3.00000, 0.30000, 2.25000),
    Vertex(-2.70000, 0.30000, 2.02500),
    Vertex(-3.00000, 0.30000, 1.80000),
    Vertex(-2.70000, 0.30000, 1.80000),
    Vertex(-2.70000, 0.00000, 1.57500),
    Vertex(-2.70000, -0.30000, 1.57500),
    Vertex(-3.00000, -0.30000, 1.35000),
    Vertex(-3.00000, 0.00000, 1.35000),
    Vertex(-2.50000, 0.00000, 1.12500),
    Vertex(-2.50000, -0.30000, 1.12500),
    Vertex(-2.65000, -0.30000, 0.93750),
    Vertex(-2.65000, 0.00000, 0.93750),
    Vertex(-2.00000, -0.30000, 0.90000),
    Vertex(-1.90000, -0.30000, 0.60000),
    Vertex(-1.90000, 0.00000, 0.60000),
    Vertex(-3.00000, 0.30000, 1.35000),
    Vertex(-2.70000, 0.30000, 1.57500),
    Vertex(-2.65000, 0.30000, 0.93750),
    Vertex(-2.50000, 0.30000, 1.12500),
    Vertex(-1.90000, 0.30000, 0.60000),
    Vertex(-2.00000, 0.30000, 0.90000),
    Vertex(1.70000, 0.00000, 1.42500),
    Vertex(1.70000, -0.66000, 1.42500),
    Vertex(1.70000, -0.66000, 0.60000),
    Vertex(1.70000, 0.00000, 0.60000),
    Vertex(2.60000, 0.00000, 1.42500),
    Vertex(2.60000, -0.66000, 1.42500),
    Vertex(3.10000, -0.66000, 0.82500),
    Vertex(3.10000, 0.00000, 0.82500),
    Vertex(2.30000, 0.00000, 2.10000),
    Vertex(2.30000, -0.25000, 2.10000),
    Vertex(2.40000, -0.25000, 2.02500),
    Vertex(2.40000, 0.00000, 2.02500),
    Vertex(2.70000, 0.00000, 2.40000),
    Vertex(2.70000, -0.25000, 2.40000),
    Vertex(3.30000, -0.25000, 2.40000),
    Vertex(3.30000, 0.00000, 2.40000),
    Vertex(1.70000, 0.66000, 0.60000),
    Vertex(1.70000, 0.66000, 1.42500),
    Vertex(3.10000, 0.66000, 0.82500),
    Vertex(2.60000, 0.66000, 1.42500),
    Vertex(2.40000, 0.25000, 2.02500),
    Vertex(2.30000, 0.25000, 2.10000),
    Vertex(3.30000, 0.25000, 2.40000),
    Vertex(2.70000, 0.25000, 2.40000),
    Vertex(2.80000, 0.00000, 2.47500),
    Vertex(2.80000, -0.25000, 2.47500),
    Vertex(3.52500, -0.25000, 2.49375),
    Vertex(3.52500, 0.00000, 2.49375),
    Vertex(2.90000, 0.00000, 2.47500),
    Vertex(2.90000, -0.15000, 2.47500),
    Vertex(3.45000, -0.15000, 2.51250),
    Vertex(3.45000, 0.00000, 2.51250),
    Vertex(2.80000, 0.00000, 2.40000),
    Vertex(2.80000, -0.15000, 2.40000),
    Vertex(3.20000, -0.15000, 2.40000),
    Vertex(3.20000, 0.00000, 2.40000),
    Vertex(3.52500, 0.25000, 2.49375),
    Vertex(2.80000, 0.25000, 2.47500),
    Vertex(3.45000, 0.15000, 2.51250),
    Vertex(2.90000, 0.15000, 2.47500),
    Vertex(3.20000, 0.15000, 2.40000),
    Vertex(2.80000, 0.15000, 2.40000),
    Vertex(0.00000, 0.00000, 3.15000),
    Vertex(0.00000, -0.00200, 3.15000),
    Vertex(0.00200, 0.00000, 3.15000),
    Vertex(0.80000, 0.00000, 3.15000),
    Vertex(0.80000, -0.45000, 3.15000),
    Vertex(0.45000, -0.80000, 3.15000),
    Vertex(0.00000, -0.80000, 3.15000),
    Vertex(0.00000, 0.00000, 2.85000),
    Vertex(0.20000, 0.00000, 2.70000),
    Vertex(0.20000, -0.11200, 2.70000),
    Vertex(0.11200, -0.20000, 2.70000),
    Vertex(0.00000, -0.20000, 2.70000),
    Vertex(-0.00200, 0.00000, 3.15000),
    Vertex(-0.45000, -0.80000, 3.15000),
    Vertex(-0.80000, -0.45000, 3.15000),
    Vertex(-0.80000, 0.00000, 3.15000),
    Vertex(-0.11200, -0.20000, 2.70000),
    Vertex(-0.20000, -0.11200, 2.70000),
    Vertex(-0.20000, 0.00000, 2.70000),
    Vertex(0.00000, 0.00200, 3.15000),
    Vertex(-0.80000, 0.45000, 3.15000),
    Vertex(-0.45000, 0.80000, 3.15000),
    Vertex(0.00000, 0.80000, 3.15000),
    Vertex(-0.20000, 0.11200, 2.70000),
    Vertex(-0.11200, 0.20000, 2.70000),
    Vertex(0.00000, 0.20000, 2.70000),
    Vertex(0.45000, 0.80000, 3.15000),
    Vertex(0.80000, 0.45000, 3.15000),
    Vertex(0.11200, 0.20000, 2.70000),
    Vertex(0.20000, 0.11200, 2.70000),
    Vertex(0.40000, 0.00000, 2.55000),
    Vertex(0.40000, -0.22400, 2.55000),
    Vertex(0.22400, -0.40000, 2.55000),
    Vertex(0.00000, -0.40000, 2.55000),
    Vertex(1.30000, 0.00000, 2.55000),
    Vertex(1.30000, -0.72800, 2.55000),
    Vertex(0.72800, -1.30000, 2.55000),
    Vertex(0.00000, -1.30000, 2.55000),
    Vertex(1.30000, 0.00000, 2.40000),
    Vertex(1.30000, -0.72800, 2.40000),
    Vertex(0.72800, -1.30000, 2.40000),
    Vertex(0.00000, -1.30000, 2.40000),
    Vertex(-0.22400, -0.40000, 2.55000),
    Vertex(-0.40000, -0.22400, 2.55000),
    Vertex(-0.40000, 0.00000, 2.55000),
    Vertex(-0.72800, -1.30000, 2.55000),
    Vertex(-1.30000, -0.72800, 2.55000),
    Vertex(-1.30000, 0.00000, 2.55000),
    Vertex(-0.72800, -1.30000, 2.40000),
    Vertex(-1.30000, -0.72800, 2.40000),
    Vertex(-1.30000, 0.00000, 2.40000),
    Vertex(-0.40000, 0.22400, 2.55000),
    Vertex(-0.22400, 0.40000, 2.55000),
    Vertex(0.00000, 0.40000, 2.55000),
    Vertex(-1.30000, 0.72800, 2.55000),
    Vertex(-0.72800, 1.30000, 2.55000),
    Vertex(0.00000, 1.30000, 2.55000),
    Vertex(-1.30000, 0.72800, 2.40000),
    Vertex(-0.72800, 1.30000, 2.40000),
    Vertex(0.00000, 1.30000, 2.40000),
    Vertex(0.22400, 0.40000, 2.55000),
    Vertex(0.40000, 0.22400, 2.55000),
    Vertex(0.72800, 1.30000, 2.55000),
    Vertex(1.30000, 0.72800, 2.55000),
    Vertex(0.72800, 1.30000, 2.40000),
    Vertex(1.30000, 0.72800, 2.40000),
    Vertex(0.00000, 0.00000, 0.00000),
    Vertex(1.50000, 0.00000, 0.15000),
    Vertex(1.50000, 0.84000, 0.15000),
    Vertex(0.84000, 1.50000, 0.15000),
    Vertex(0.00000, 1.50000, 0.15000),
    Vertex(1.50000, 0.00000, 0.07500),
    Vertex(1.50000, 0.84000, 0.07500),
    Vertex(0.84000, 1.50000, 0.07500),
    Vertex(0.00000, 1.50000, 0.07500),
    Vertex(1.42500, 0.00000, 0.00000),
    Vertex(1.42500, 0.79800, 0.00000),
    Vertex(0.79800, 1.42500, 0.00000),
    Vertex(0.00000, 1.42500, 0.00000),
    Vertex(-0.84000, 1.50000, 0.15000),
    Vertex(-1.50000, 0.84000, 0.15000),
    Vertex(-1.50000, 0.00000, 0.15000),
    Vertex(-0.84000, 1.50000, 0.07500),
    Vertex(-1.50000, 0.84000, 0.07500),
    Vertex(-1.50000, 0.00000, 0.07500),
    Vertex(-0.79800, 1.42500, 0.00000),
    Vertex(-1.42500, 0.79800, 0.00000),
    Vertex(-1.42500, 0.00000, 0.00000),
    Vertex(-1.50000, -0.84000, 0.15000),
    Vertex(-0.84000, -1.50000, 0.15000),
    Vertex(0.00000, -1.50000, 0.15000),
    Vertex(-1.50000, -0.84000, 0.07500),
    Vertex(-0.84000, -1.50000, 0.07500),
    Vertex(0.00000, -1.50000, 0.07500),
    Vertex(-1.42500, -0.79800, 0.00000),
    Vertex(-0.79800, -1.42500, 0.00000),
    Vertex(0.00000, -1.42500, 0.00000),
    Vertex(0.84000, -1.50000, 0.15000),
    Vertex(1.50000, -0.84000, 0.15000),
    Vertex(0.84000, -1.50000, 0.07500),
    Vertex(1.50000, -0.84000, 0.07500),
    Vertex(0.79800, -1.42500, 0.00000),
    Vertex(1.42500, -0.79800, 0.00000)
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List()
}