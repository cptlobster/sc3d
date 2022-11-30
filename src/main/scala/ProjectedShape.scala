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

import scala.math.{abs, round}
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters._

case class ProjectedShape(points: ParArray[Array[Double]], edges: ParSeq[(Int, Int)]) {
  private def drawLine(arr: ParArray[ParArray[Boolean]], p1: Array[Int], p2: Array[Int]): ParArray[ParArray[Boolean]] = {
    var rx: Int = p1(0)
    var ry: Int = p1(1)
    val dx: Double = p2(0) - p1(0)
    val dy: Double = p2(1) - p1(1)

    if (dx != 0 && abs(dy / dx) <= 1) {
      val yi: Double = ry - (dy / dx) * rx
      def f(xi: Int): Double = (dy / dx) * xi + yi
      while (rx != p2(0)) {
        val y: Int = round(f(rx)).toInt
        if ((0 until arr.size).contains(y)) if ((0 until arr(y).size).contains(rx)) arr(y)(rx) = true
        if (dx >= 0) rx += 1 else rx -= 1
      }
    }
    else if (dy != 0 && abs(dx / dy) < 1) {
      val xi: Double = rx - (dx / dy) * ry
      def f(yi: Int): Double = (dx / dy) * yi + xi
      while (ry != p2(1)) {
        val x: Int = round(f(ry)).toInt
        if ((0 until arr.size).contains(ry)) if ((0 until arr(ry).size).contains(x)) arr(ry)(x) = true
        if (dy >= 0) ry += 1 else ry -= 1
      }
    }
    arr
  }

  def rasterize(rows: Int, cols: Int, mx: Int, my: Int): ParArray[ParArray[Boolean]] = {
    var arr: ParArray[ParArray[Boolean]] = Array.ofDim[Boolean](rows, cols).par.map(_.par)
    val ints: ParArray[Array[Int]] = points.map(a => Array(round(a(0) * mx).toInt + (cols / 2), round(a(1) * my).toInt + (rows / 2)))
    for (i <- ints) {
      val x: Int = i(0)
      val y: Int = i(1)
      if ((0 until arr.size).contains(y)) if ((0 until arr(y).size).contains(x)) arr(y)(x) = true
    }
    for (i <- edges) { arr = drawLine(arr, ints(i._1), ints(i._2)) }
    arr
  }
}
