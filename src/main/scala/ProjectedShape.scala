/*
 *   Scala 3D renderer - 2D projected shapes for text-based projector
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

import scala.math.round

case class ProjectedShape(points: Array[Array[Double]], edges: List[(Int, Int)]) {
  private def drawLine(arr: Array[Array[Boolean]], p1: Array[Int], p2: Array[Int]): Array[Array[Boolean]] = {
    var rx: Int = p1(0)
    var ry: Int = p1(1)
    val dx: Double = p2(0) - p1(0)
    val dy: Double = p2(1) - p1(1)
    def si(mn: Double, md: Double, rv: Double, ri: Double): Int = {
      val i: Double = ri - (mn/md) * rv
      round((mn / md) * rv + i).toInt
    }
    if (dx != 0 && Math.abs(dy / dx) <= 1) {
      while (rx != p2(0)) {
        val y: Int = si(dy, dx, rx, ry)
        if (arr.indices.contains(y)) if (arr(y).indices.contains(rx)) arr(y)(rx) = true
        if (dx >= 0) rx += 1 else rx -= 1
      }
    }
    else if (dy != 0 && Math.abs(dx / dy) < 1) {
      while (ry != p2(1)) {
        val x: Int = si(dx, dy, ry, rx)
        if (arr.indices.contains(ry)) if (arr(ry).indices.contains(x)) arr(ry)(x) = true
        if (dy >= 0) ry += 1 else ry -= 1
      }
    }
    arr
  }

  def rasterize(rows: Int, cols: Int, mx: Int, my: Int): Array[Array[Boolean]] = {
    var arr: Array[Array[Boolean]] = Array.ofDim(rows, cols)
    val ints: Array[Array[Int]] = points.map(a => Array(round(a(0) * mx).toInt + (cols / 2), round(a(1) * my).toInt + (rows / 2)))
    for (i <- ints) {
      val x: Int = i(0)
      val y: Int = i(1)
      if (arr.indices.contains(y)) if (arr(y).indices.contains(x)) arr(y)(x) = true
    }
    for (i <- edges) { arr = drawLine(arr, ints(i._1), ints(i._2)) }
    arr
  }
}
