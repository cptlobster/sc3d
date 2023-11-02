package dev.cptlobster.sc3d
package core

import org.scalatest.funsuite.AnyFunSuite

class VertexTest extends AnyFunSuite:
  test("Vertex.values") {
    val v1: Vertex = new Vertex(4, 2, 0)
    val v2: Vertex = Vertex.up
    val v3: Vertex = Vertex.zero

    assert(v1.x == 4)
    assert(v1.y == 2)
    assert(v2.y == 1)
    assert(v3.x == 0 && v3.y == 0 && v3.z == 0)
  }

  test("Vertex.toString") {
    val v1: Vertex = new Vertex(1, 2, 3)
    assert(v1.toString == "(1.0, 2.0, 3.0)")
  }

  test("Vertex.normalize") {
    val v1: Vertex = new Vertex(1, 2, 3)
    val v1_n: Vertex = v1.normalize

    assert(v1_n.z == 1.0)
    assert(v1_n.x == 1.0 / 3.0)
    assert(v1_n.y == 2.0 / 3.0)
  }