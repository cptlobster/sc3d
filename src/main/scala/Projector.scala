package org.cptlobster

import scala.math.Pi

class Projector {
  /*
   * This is where you add shapes. Simply append them to the list. List of available shapes in `Shape.scala`
   */
  var shp: List[Shape] = List(Cube(4), Cube(2), Pyramid(1))

  var cam: Camera = Camera()
  /*
   * If you so desire, you can move the camera with the following:
   */
  cam.pos += new Vertex(0, 0, 15)
  cam.rot += new Vertex(0, 0, 0)
  cam.plane += new Vertex(0, 0, 8)

  /*
   * Almost everything past here is hell. Enter at your own risk.
   * If you want to control how shapes rotate, jump down to `main.vectors` and edit items in that list.
   */

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

  /*
   * Make shapes spin.
   */
  def vectors: List[(Double, Double, Double)] = List(
    (0, Pi / 16, -Pi / 32),
    (-Pi / 16, Pi / 32, 0),
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