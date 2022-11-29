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

package org.cptlobster;

class Renderer(rows: Int = 60, cols: Int = 200, mult: Int = 10) {
  def blank_screen(): Array[Array[String]] = (for (_ <- 1 to rows) yield {
    (for (_ <- 1 to cols) yield {
      " "
    }).toArray
  }).toArray

  def display(points: List[ProjectedShape]): Unit = {
    val scr: Array[Array[String]] = blank_screen()
    val proj = points.map(_.rasterize(rows, cols, mult * 2, mult))
    for (l <- proj) {
      for (i <- scr.indices; j <- scr(i).indices) {
        if (l(i)(j)) scr(i)(j) = "#"
      }
    }
    print("\u001b[H" + scr.map(_.mkString("")).mkString("\n"))
  }
}
