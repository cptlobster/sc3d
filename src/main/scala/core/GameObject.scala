/*
 *   Scala 3D renderer - GameObject class
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

case class GameObject(transform: Transform, modules: List[Component]) {
  /**
   * Tick this [[GameObject]].
   * === Explanation ===
   * Our [[GameObject]] will usually have some [[Component]]s attached to it. Each of these has [[Component.on_update]],
   * a function that takes a [[GameObject]] and a `delta`, and returns a [[GameObject]]. Because of this design, we can
   * just chain each [[Component.on_update]], which makes using a [[foldLeft]] ideal.
   *
   * @return
   */
  def on_update(delta: Double): GameObject = modules.foldLeft(this){ (obj, mod) => mod.on_update(obj, delta) }
}
