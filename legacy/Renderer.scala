/*
 *   Scala 3D renderer - text-based wireframe renderer
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

import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters._

class Renderer(rows: Int = 60, cols: Int = 200, mult: Int = 10) {
  def blank_screen(): ParArray[ParArray[String]] = (for (_ <- 1 to rows) yield {
    (for (_ <- 1 to cols) yield {
      " "
    }).toArray.par
  }).toArray.par

  def display(points: ParSeq[ProjectedShape]): Unit = {
    val scr: ParArray[ParArray[String]] = blank_screen()
    val proj = points.map(_.rasterize(rows, cols, mult * 2, mult))
    for (l <- proj) {
      for (i <- 0 until scr.size; j <- 0 until scr(i).size) {
        if (l(i)(j)) scr(i)(j) = "#"
      }
    }
    print("\u001b[H" + scr.map(_.mkString("")).mkString("\n"))
  }
}
