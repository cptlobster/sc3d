package org.cptlobster

trait Transformable {
  var pos: Vertex = Vertex(0, 0, 0) // position
  var rot: Vertex = Vertex(0, 0, 0) // rotation
  var scl: Vertex = Vertex(1, 1, 1) // scale
  override def toString: String = s"P: $pos; R: $rot; S: $scl"
}