/*
 *   Scala 3D renderer - common trait for GameObject components
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

/**
 * Basically, each [[GameObject]] contains a [[List]] of [[Component]]s, that manipulate [[GameObject]]s in some way.
 * These [[Component]]s are things like physics controllers, shape renderers, and user-created behavioral scripts.
 * Any functions that interact with the [[GameObject]] take it as an input and returns it when it's finished.
 *
 * This allows for very efficient (and nice looking) implementation where we pass the [[GameObject]] through all of its
 * components:
 *
 * {{{
 * def on_update(delta: Double): GameObject = modules.foldLeft(this){ (obj, mod) => mod.on_update(obj, delta) }
 * }}}
 *
 * martin odersky would probably kill me for this
 */
trait Component {
  def on_create(source: GameObject): GameObject
  def on_update(source: GameObject, delta: Double): GameObject
}
