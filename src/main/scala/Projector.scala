package org.cptlobster

import scala.math.Pi

class Projector {
  //var shp: List[Shape] = List(Cube(4), Cube(2), Pyramid(1))
  var shp: List[Shape] = List(Utah(1))
  /* var shp: List[Shape] =
  List(
    Poly(
      Array(
        new Vertex(1, 0, 0),
        new Vertex(-1, 0, 0)
      ),
      List((0, 1))
    )
  )*/

  var cam: Camera = Camera()

  def projection: List[ProjectedShape] = {
    shp.map(_.project(cam))
  }
}

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

object Main {
  def rotate_shape(shape: Shape, x: Double, y: Double, z: Double): Shape = {
    shape.rot = new Vertex(shape.rot.x + x, shape.rot.y + y, shape.rot.z + z)
    shape
  }

  def vectors: List[(Double, Double, Double)] = List(
    // (0, Pi / 16, -Pi / 32),
    // (-Pi / 16, Pi / 32, 0),
    (0, Pi / 64, 0)
  )

  def main(args: Array[String]): Unit = {
    val proj: Projector = new Projector()
    val rend: Renderer = new Renderer()
    print("\u001b[?25l")
    while (true) {
      rend.display(proj.projection)
      Thread.sleep(50)
      proj.shp.zip(vectors).map(p => rotate_shape(p._1, p._2._1, p._2._2, p._2._3))
    }
  }
}