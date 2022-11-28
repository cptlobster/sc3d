package org.cptlobster

import scala.math.{Pi, abs, atan2, cos, round, sin}

/*
 * List of classes:
 *  - `Vertex`: Base vertex class, a point in 3 dimensional space.
 *  - `Transformable`: Adds basic transforms to elements.
 *  - `Camera`: Camera object.
 *  - `ProjectedShape`: Used for 3D->2D rendering. Probably will be moved to `Projector.scala` at some point.
 *  - `Shape`: Base case class of all shapes.
 *  - `Cube`: Pre-mapped points and lines for a cube. `r` controls size.
 *  - `Pyramid`: Pre-mapped points and lines for a pyramid. `r` controls size.
 *  - `Sphere`: Experiment with auto-generated points for a sphere. `r` controls radius, `p` controls number of points per axis.
 *  - `Utah : A certified 3D-modeling classic. `r` controls size.
 */


class Vertex(val x: Double, val y: Double, val z: Double) {
  def asArray: Array[Double] = Array(x, y, z)
  override def toString: String = s"($x, $y, $z)"
  def + (r : Vertex): Vertex = new Vertex(x + r.x, y + r.y, z + r.z)
  def - (r : Vertex): Vertex = new Vertex(x - r.x, y - r.y, z - r.z)
  def * (r : Double): Vertex = new Vertex(x * r, y * r, z * r)
  def * (r : Vertex): Vertex = new Vertex(x * r.x, y * r.y, z * r.z)
  def / (r : Double): Vertex = new Vertex(x / r, y / r, z / r)
  def / (r : Vertex): Vertex = new Vertex(x / r.x, y / r.y, z / r.z)
}

trait Transformable {
  var pos: Vertex = new Vertex(0, 0, 0)
  var rot: Vertex = new Vertex(0, 0, 0)

  override def toString: String = s"P: $pos; R: $rot"
}

case class Camera(var p: Vertex = new Vertex(0, 0, 0),
                  var r: Vertex = new Vertex (0, 0, 0),
                  var plane: Vertex = new Vertex (0, 0, 0)) extends Transformable {
  pos = p
  rot = r
}

case class ProjectedShape(points: Array[Array[Double]], edges: List[(Int, Int)]) {
  private def drawLine(arr: Array[Array[Boolean]], p1: Array[Int], p2: Array[Int]): Array[Array[Boolean]] = {
    var rx: Int = p1(0)
    var ry: Int = p1(1)
    val dx: Double = p2(0) - p1(0)
    val dy: Double = p2(1) - p1(1)
    if (dx != 0 && Math.abs(dy / dx) <= 1) {
      val iy: Double = ry - (dy/dx) * rx
      def f(xi: Int): Double = (dy/dx) * xi + iy
      while (rx != p2(0)) {
        val y: Int = round(f(rx)).toInt
        if (arr.indices.contains(y)) if (arr(y).indices.contains(rx)) arr(y)(rx) = true
        if (dx >= 0) rx += 1 else rx -= 1
      }
    }
    else if (dy != 0 && Math.abs(dx / dy) < 1) {
      val ix: Double = rx - (dx/dy) * ry
      def f(yi: Int): Double = (dx/dy) * yi + ix

      while (ry != p2(1)) {
        val x: Int = round(f(ry)).toInt
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

abstract class Shape extends Transformable {
  val points: Array[Vertex]
  val edges: List[(Int, Int)]

  private def transformArr(a: Array[Array[Double]]): Array[Array[Double]] = {
    val rows = a.length
    val cols = a.head.length
    val trans: Array[Array[Double]] = Array.ofDim(cols, rows)

    for (i <- 0 until cols; j <- 0 until rows) {
      trans(i)(j) = a(j)(i)
    }
    trans
  }

  private def matMult(a1: Array[Vertex], a2: Array[Array[Double]]): Array[Vertex] = {
    val a2t = transformArr(a2)

    val rows = a1.length
    val trans: Array[Vertex] = Array.ofDim(rows)

    for (i <- 0 until rows) {
      trans(i) = new Vertex(
        a1(i).asArray.zip(a2t(0)).map(a => a._1 * a._2).sum,
        a1(i).asArray.zip(a2t(1)).map(a => a._1 * a._2).sum,
        a1(i).asArray.zip(a2t(2)).map(a => a._1 * a._2).sum
      )
    }
    trans
  }

  def translate(a: Array[Vertex], x: Double, y: Double, z: Double): Array[Vertex] = {
    a.map(v => new Vertex(v.x + x, v.y + y, v.z + z))
  }

  def rotate(a: Array[Vertex], rx: Double, ry: Double, rz: Double): Array[Vertex] = {
    val rtx: Array[Array[Double]] = Array(
      Array(1, 0, 0),
      Array(0, cos(rx), sin(rx)),
      Array(0, -sin(rx), cos(rx))
    )
    val rty: Array[Array[Double]] = Array(
      Array(cos(ry), 0, -sin(ry)),
      Array(0, 1, 0),
      Array(sin(ry), 0, cos(ry))
    )
    val rtz: Array[Array[Double]] = Array(
      Array(cos(rz), sin(rz), 0),
      Array(-sin(rz), cos(rz), 0),
      Array(0, 0, 1)
    )
    matMult(matMult(matMult(a, rtx), rty), rtz)
  }

  def transform: Array[Vertex] = {
    val x = this.pos.x
    val y = this.pos.y
    val z = this.pos.z
    val rx = this.rot.x
    val ry = this.rot.y
    val rz = this.rot.z
    translate(rotate(this.points, rx, ry, rz), x, y, z)
  }

  def project(c: Camera): ProjectedShape = {
    val ex = c.plane.x
    val ey = c.plane.y
    val ez = c.plane.z
    val adj_pts: Array[Vertex] = translate(transform, -c.pos.x, -c.pos.y, -c.pos.z)

    ProjectedShape(adj_pts.map(a => Array(((ez / a.z) * a.x) + ex, ((ez / a.z) * a.y) + ey)), edges)
  }
}

case class Poly(points: Array[Vertex], edges: List[(Int, Int)]) extends Shape {

}

case class Cube(r: Double) extends Shape {
  override val points: Array[Vertex] = Array(
    new Vertex(1, 1, 1), // 0, A
    new Vertex(-1, 1, 1), // 1, B
    new Vertex(1, -1, 1), // 2, C
    new Vertex(1, 1, -1), // 3, D
    new Vertex(1, -1, -1), // 4, E
    new Vertex(-1, 1, -1), // 5, F
    new Vertex(-1, -1, 1), // 6, G
    new Vertex(-1, -1, -1) // 7, H
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List(
    (0, 1),
    (0, 2),
    (0, 3),
    (1, 5),
    (1, 6),
    (2, 4),
    (2, 6),
    (3, 4),
    (3, 5),
    (4, 7),
    (5, 7),
    (6, 7)
  )
}

case class Pyramid(r: Double) extends Shape {
  override val points: Array[Vertex] = Array(
    new Vertex(0, 1, 0), // 0
    new Vertex(-1, -1, -1), // 1
    new Vertex(-1, -1, 1), // 2
    new Vertex(1, -1, -1), // 3
    new Vertex(1, -1, 1) // 4
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List(
    (0, 1),
    (0, 2),
    (0, 3),
    (0, 4),
    (1, 2),
    (1, 3),
    (2, 3),
    (3, 4)
  )
}

case class Sphere(r: Double, p: Int) extends Shape {
  override val points: Array[Vertex] = (for (i <- 0 until 2 * p; j <- 0 until p) yield {
    val a1: Double = i * Pi / p
    val a2: Double = j * Pi / p
    new Vertex(sin(a1) * cos(a2), sin(a1) * sin(a2), cos(a1)) * r
  }).toArray

  override val edges: List[(Int, Int)] = List()
}

case class Utah(r: Double) extends Shape {
  rot += new Vertex(-Pi / 2, 0, 0)

  override val points: Array[Vertex] = Array(
    new Vertex(1.40000, 0.00000, 2.40000),
    new Vertex(1.40000, -0.78400, 2.40000),
    new Vertex(0.78000, -1.40000, 2.40000),
    new Vertex(0.00000, -1.40000, 2.40000),
    new Vertex(1.33750, 0.00000, 2.53125),
    new Vertex(1.33750, -0.74900, 2.53125),
    new Vertex(0.74900, -1.33750, 2.53125),
    new Vertex(0.00000, -1.33750, 2.53125),
    new Vertex(1.43750, 0.00000, 2.53125),
    new Vertex(1.43750, -0.80500, 2.53125),
    new Vertex(0.80500, -1.43750, 2.53125),
    new Vertex(0.00000, -1.43750, 2.53125),
    new Vertex(1.50000, 0.00000, 2.40000),
    new Vertex(1.50000, -0.84000, 2.40000),
    new Vertex(0.84000, -1.50000, 2.40000),
    new Vertex(0.00000, -1.50000, 2.40000),
    new Vertex(-0.78400, -1.40000, 2.40000),
    new Vertex(-1.40000, -0.78400, 2.40000),
    new Vertex(-1.40000, 0.00000, 2.40000),
    new Vertex(-0.74900, -1.33750, 2.53125),
    new Vertex(-1.33750, -0.74900, 2.53125),
    new Vertex(-1.33750, 0.00000, 2.53125),
    new Vertex(-0.80500, -1.43750, 2.53125),
    new Vertex(-1.43750, -0.80500, 2.53125),
    new Vertex(-1.43750, 0.00000, 2.53125),
    new Vertex(-0.84000, -1.50000, 2.40000),
    new Vertex(-1.50000, -0.84000, 2.40000),
    new Vertex(-1.50000, 0.00000, 2.40000),
    new Vertex(-1.40000, 0.78400, 2.40000),
    new Vertex(-0.78400, 1.40000, 2.40000),
    new Vertex(0.00000, 1.40000, 2.40000),
    new Vertex(-1.33750, 0.74900, 2.53125),
    new Vertex(-0.74900, 1.33750, 2.53125),
    new Vertex(0.00000, 1.33750, 2.53125),
    new Vertex(-1.43750, 0.80500, 2.53125),
    new Vertex(-0.80500, 1.43750, 2.53125),
    new Vertex(0.00000, 1.43750, 2.53125),
    new Vertex(-1.50000, 0.84000, 2.40000),
    new Vertex(-0.84000, 1.50000, 2.40000),
    new Vertex(0.00000, 1.50000, 2.40000),
    new Vertex(0.78400, 1.40000, 2.40000),
    new Vertex(1.40000, 0.78400, 2.40000),
    new Vertex(0.74900, 1.33750, 2.53125),
    new Vertex(1.33750, 0.74900, 2.53125),
    new Vertex(0.80500, 1.43750, 2.53125),
    new Vertex(1.43750, 0.80500, 2.53125),
    new Vertex(0.84000, 1.50000, 2.40000),
    new Vertex(1.50000, 0.84000, 2.40000),
    new Vertex(1.75000, 0.00000, 1.87500),
    new Vertex(1.75000, -0.98000, 1.87500),
    new Vertex(0.98000, -1.75000, 1.87500),
    new Vertex(0.00000, -1.75000, 1.87500),
    new Vertex(2.00000, 0.00000, 1.35000),
    new Vertex(2.00000, -1.12000, 1.35000),
    new Vertex(1.12000, -2.00000, 1.35000),
    new Vertex(0.00000, -2.00000, 1.35000),
    new Vertex(2.00000, 0.00000, 0.90000),
    new Vertex(2.00000, -1.12000, 0.90000),
    new Vertex(1.12000, -2.00000, 0.90000),
    new Vertex(0.00000, -2.00000, 0.90000),
    new Vertex(-0.98000, -1.75000, 1.87500),
    new Vertex(-1.75000, -0.98000, 1.87500),
    new Vertex(-1.75000, 0.00000, 1.87500),
    new Vertex(-1.12000, -2.00000, 1.35000),
    new Vertex(-2.00000, -1.12000, 1.35000),
    new Vertex(-2.00000, 0.00000, 1.35000),
    new Vertex(-1.12000, -2.00000, 0.90000),
    new Vertex(-2.00000, -1.12000, 0.90000),
    new Vertex(-2.00000, 0.00000, 0.90000),
    new Vertex(-1.75000, 0.98000, 1.87500),
    new Vertex(-0.98000, 1.75000, 1.87500),
    new Vertex(0.00000, 1.75000, 1.87500),
    new Vertex(-2.00000, 1.12000, 1.35000),
    new Vertex(-1.12000, 2.00000, 1.35000),
    new Vertex(0.00000, 2.00000, 1.35000),
    new Vertex(-2.00000, 1.12000, 0.90000),
    new Vertex(-1.12000, 2.00000, 0.90000),
    new Vertex(0.00000, 2.00000, 0.90000),
    new Vertex(0.98000, 1.75000, 1.87500),
    new Vertex(1.75000, 0.98000, 1.87500),
    new Vertex(1.12000, 2.00000, 1.35000),
    new Vertex(2.00000, 1.12000, 1.35000),
    new Vertex(1.12000, 2.00000, 0.90000),
    new Vertex(2.00000, 1.12000, 0.90000),
    new Vertex(2.00000, 0.00000, 0.45000),
    new Vertex(2.00000, -1.12000, 0.45000),
    new Vertex(1.12000, -2.00000, 0.45000),
    new Vertex(0.00000, -2.00000, 0.45000),
    new Vertex(1.50000, 0.00000, 0.22500),
    new Vertex(1.50000, -0.84000, 0.22500),
    new Vertex(0.84000, -1.50000, 0.22500),
    new Vertex(0.00000, -1.50000, 0.22500),
    new Vertex(1.50000, 0.00000, 0.15000),
    new Vertex(1.50000, -0.84000, 0.15000),
    new Vertex(0.84000, -1.50000, 0.15000),
    new Vertex(0.00000, -1.50000, 0.15000),
    new Vertex(-1.12000, -2.00000, 0.45000),
    new Vertex(-2.00000, -1.12000, 0.45000),
    new Vertex(-2.00000, 0.00000, 0.45000),
    new Vertex(-0.84000, -1.50000, 0.22500),
    new Vertex(-1.50000, -0.84000, 0.22500),
    new Vertex(-1.50000, 0.00000, 0.22500),
    new Vertex(-0.84000, -1.50000, 0.15000),
    new Vertex(-1.50000, -0.84000, 0.15000),
    new Vertex(-1.50000, 0.00000, 0.15000),
    new Vertex(-2.00000, 1.12000, 0.45000),
    new Vertex(-1.12000, 2.00000, 0.45000),
    new Vertex(0.00000, 2.00000, 0.45000),
    new Vertex(-1.50000, 0.84000, 0.22500),
    new Vertex(-0.84000, 1.50000, 0.22500),
    new Vertex(0.00000, 1.50000, 0.22500),
    new Vertex(-1.50000, 0.84000, 0.15000),
    new Vertex(-0.84000, 1.50000, 0.15000),
    new Vertex(0.00000, 1.50000, 0.15000),
    new Vertex(1.12000, 2.00000, 0.45000),
    new Vertex(2.00000, 1.12000, 0.45000),
    new Vertex(0.84000, 1.50000, 0.22500),
    new Vertex(1.50000, 0.84000, 0.22500),
    new Vertex(0.84000, 1.50000, 0.15000),
    new Vertex(1.50000, 0.84000, 0.15000),
    new Vertex(-1.60000, 0.00000, 2.02500),
    new Vertex(-1.60000, -0.30000, 2.02500),
    new Vertex(-1.50000, -0.30000, 2.25000),
    new Vertex(-1.50000, 0.00000, 2.25000),
    new Vertex(-2.30000, 0.00000, 2.02500),
    new Vertex(-2.30000, -0.30000, 2.02500),
    new Vertex(-2.50000, -0.30000, 2.25000),
    new Vertex(-2.50000, 0.00000, 2.25000),
    new Vertex(-2.70000, 0.00000, 2.02500),
    new Vertex(-2.70000, -0.30000, 2.02500),
    new Vertex(-3.00000, -0.30000, 2.25000),
    new Vertex(-3.00000, 0.00000, 2.25000),
    new Vertex(-2.70000, 0.00000, 1.80000),
    new Vertex(-2.70000, -0.30000, 1.80000),
    new Vertex(-3.00000, -0.30000, 1.80000),
    new Vertex(-3.00000, 0.00000, 1.80000),
    new Vertex(-1.50000, 0.30000, 2.25000),
    new Vertex(-1.60000, 0.30000, 2.02500),
    new Vertex(-2.50000, 0.30000, 2.25000),
    new Vertex(-2.30000, 0.30000, 2.02500),
    new Vertex(-3.00000, 0.30000, 2.25000),
    new Vertex(-2.70000, 0.30000, 2.02500),
    new Vertex(-3.00000, 0.30000, 1.80000),
    new Vertex(-2.70000, 0.30000, 1.80000),
    new Vertex(-2.70000, 0.00000, 1.57500),
    new Vertex(-2.70000, -0.30000, 1.57500),
    new Vertex(-3.00000, -0.30000, 1.35000),
    new Vertex(-3.00000, 0.00000, 1.35000),
    new Vertex(-2.50000, 0.00000, 1.12500),
    new Vertex(-2.50000, -0.30000, 1.12500),
    new Vertex(-2.65000, -0.30000, 0.93750),
    new Vertex(-2.65000, 0.00000, 0.93750),
    new Vertex(-2.00000, -0.30000, 0.90000),
    new Vertex(-1.90000, -0.30000, 0.60000),
    new Vertex(-1.90000, 0.00000, 0.60000),
    new Vertex(-3.00000, 0.30000, 1.35000),
    new Vertex(-2.70000, 0.30000, 1.57500),
    new Vertex(-2.65000, 0.30000, 0.93750),
    new Vertex(-2.50000, 0.30000, 1.12500),
    new Vertex(-1.90000, 0.30000, 0.60000),
    new Vertex(-2.00000, 0.30000, 0.90000),
    new Vertex(1.70000, 0.00000, 1.42500),
    new Vertex(1.70000, -0.66000, 1.42500),
    new Vertex(1.70000, -0.66000, 0.60000),
    new Vertex(1.70000, 0.00000, 0.60000),
    new Vertex(2.60000, 0.00000, 1.42500),
    new Vertex(2.60000, -0.66000, 1.42500),
    new Vertex(3.10000, -0.66000, 0.82500),
    new Vertex(3.10000, 0.00000, 0.82500),
    new Vertex(2.30000, 0.00000, 2.10000),
    new Vertex(2.30000, -0.25000, 2.10000),
    new Vertex(2.40000, -0.25000, 2.02500),
    new Vertex(2.40000, 0.00000, 2.02500),
    new Vertex(2.70000, 0.00000, 2.40000),
    new Vertex(2.70000, -0.25000, 2.40000),
    new Vertex(3.30000, -0.25000, 2.40000),
    new Vertex(3.30000, 0.00000, 2.40000),
    new Vertex(1.70000, 0.66000, 0.60000),
    new Vertex(1.70000, 0.66000, 1.42500),
    new Vertex(3.10000, 0.66000, 0.82500),
    new Vertex(2.60000, 0.66000, 1.42500),
    new Vertex(2.40000, 0.25000, 2.02500),
    new Vertex(2.30000, 0.25000, 2.10000),
    new Vertex(3.30000, 0.25000, 2.40000),
    new Vertex(2.70000, 0.25000, 2.40000),
    new Vertex(2.80000, 0.00000, 2.47500),
    new Vertex(2.80000, -0.25000, 2.47500),
    new Vertex(3.52500, -0.25000, 2.49375),
    new Vertex(3.52500, 0.00000, 2.49375),
    new Vertex(2.90000, 0.00000, 2.47500),
    new Vertex(2.90000, -0.15000, 2.47500),
    new Vertex(3.45000, -0.15000, 2.51250),
    new Vertex(3.45000, 0.00000, 2.51250),
    new Vertex(2.80000, 0.00000, 2.40000),
    new Vertex(2.80000, -0.15000, 2.40000),
    new Vertex(3.20000, -0.15000, 2.40000),
    new Vertex(3.20000, 0.00000, 2.40000),
    new Vertex(3.52500, 0.25000, 2.49375),
    new Vertex(2.80000, 0.25000, 2.47500),
    new Vertex(3.45000, 0.15000, 2.51250),
    new Vertex(2.90000, 0.15000, 2.47500),
    new Vertex(3.20000, 0.15000, 2.40000),
    new Vertex(2.80000, 0.15000, 2.40000),
    new Vertex(0.00000, 0.00000, 3.15000),
    new Vertex(0.00000, -0.00200, 3.15000),
    new Vertex(0.00200, 0.00000, 3.15000),
    new Vertex(0.80000, 0.00000, 3.15000),
    new Vertex(0.80000, -0.45000, 3.15000),
    new Vertex(0.45000, -0.80000, 3.15000),
    new Vertex(0.00000, -0.80000, 3.15000),
    new Vertex(0.00000, 0.00000, 2.85000),
    new Vertex(0.20000, 0.00000, 2.70000),
    new Vertex(0.20000, -0.11200, 2.70000),
    new Vertex(0.11200, -0.20000, 2.70000),
    new Vertex(0.00000, -0.20000, 2.70000),
    new Vertex(-0.00200, 0.00000, 3.15000),
    new Vertex(-0.45000, -0.80000, 3.15000),
    new Vertex(-0.80000, -0.45000, 3.15000),
    new Vertex(-0.80000, 0.00000, 3.15000),
    new Vertex(-0.11200, -0.20000, 2.70000),
    new Vertex(-0.20000, -0.11200, 2.70000),
    new Vertex(-0.20000, 0.00000, 2.70000),
    new Vertex(0.00000, 0.00200, 3.15000),
    new Vertex(-0.80000, 0.45000, 3.15000),
    new Vertex(-0.45000, 0.80000, 3.15000),
    new Vertex(0.00000, 0.80000, 3.15000),
    new Vertex(-0.20000, 0.11200, 2.70000),
    new Vertex(-0.11200, 0.20000, 2.70000),
    new Vertex(0.00000, 0.20000, 2.70000),
    new Vertex(0.45000, 0.80000, 3.15000),
    new Vertex(0.80000, 0.45000, 3.15000),
    new Vertex(0.11200, 0.20000, 2.70000),
    new Vertex(0.20000, 0.11200, 2.70000),
    new Vertex(0.40000, 0.00000, 2.55000),
    new Vertex(0.40000, -0.22400, 2.55000),
    new Vertex(0.22400, -0.40000, 2.55000),
    new Vertex(0.00000, -0.40000, 2.55000),
    new Vertex(1.30000, 0.00000, 2.55000),
    new Vertex(1.30000, -0.72800, 2.55000),
    new Vertex(0.72800, -1.30000, 2.55000),
    new Vertex(0.00000, -1.30000, 2.55000),
    new Vertex(1.30000, 0.00000, 2.40000),
    new Vertex(1.30000, -0.72800, 2.40000),
    new Vertex(0.72800, -1.30000, 2.40000),
    new Vertex(0.00000, -1.30000, 2.40000),
    new Vertex(-0.22400, -0.40000, 2.55000),
    new Vertex(-0.40000, -0.22400, 2.55000),
    new Vertex(-0.40000, 0.00000, 2.55000),
    new Vertex(-0.72800, -1.30000, 2.55000),
    new Vertex(-1.30000, -0.72800, 2.55000),
    new Vertex(-1.30000, 0.00000, 2.55000),
    new Vertex(-0.72800, -1.30000, 2.40000),
    new Vertex(-1.30000, -0.72800, 2.40000),
    new Vertex(-1.30000, 0.00000, 2.40000),
    new Vertex(-0.40000, 0.22400, 2.55000),
    new Vertex(-0.22400, 0.40000, 2.55000),
    new Vertex(0.00000, 0.40000, 2.55000),
    new Vertex(-1.30000, 0.72800, 2.55000),
    new Vertex(-0.72800, 1.30000, 2.55000),
    new Vertex(0.00000, 1.30000, 2.55000),
    new Vertex(-1.30000, 0.72800, 2.40000),
    new Vertex(-0.72800, 1.30000, 2.40000),
    new Vertex(0.00000, 1.30000, 2.40000),
    new Vertex(0.22400, 0.40000, 2.55000),
    new Vertex(0.40000, 0.22400, 2.55000),
    new Vertex(0.72800, 1.30000, 2.55000),
    new Vertex(1.30000, 0.72800, 2.55000),
    new Vertex(0.72800, 1.30000, 2.40000),
    new Vertex(1.30000, 0.72800, 2.40000),
    new Vertex(0.00000, 0.00000, 0.00000),
    new Vertex(1.50000, 0.00000, 0.15000),
    new Vertex(1.50000, 0.84000, 0.15000),
    new Vertex(0.84000, 1.50000, 0.15000),
    new Vertex(0.00000, 1.50000, 0.15000),
    new Vertex(1.50000, 0.00000, 0.07500),
    new Vertex(1.50000, 0.84000, 0.07500),
    new Vertex(0.84000, 1.50000, 0.07500),
    new Vertex(0.00000, 1.50000, 0.07500),
    new Vertex(1.42500, 0.00000, 0.00000),
    new Vertex(1.42500, 0.79800, 0.00000),
    new Vertex(0.79800, 1.42500, 0.00000),
    new Vertex(0.00000, 1.42500, 0.00000),
    new Vertex(-0.84000, 1.50000, 0.15000),
    new Vertex(-1.50000, 0.84000, 0.15000),
    new Vertex(-1.50000, 0.00000, 0.15000),
    new Vertex(-0.84000, 1.50000, 0.07500),
    new Vertex(-1.50000, 0.84000, 0.07500),
    new Vertex(-1.50000, 0.00000, 0.07500),
    new Vertex(-0.79800, 1.42500, 0.00000),
    new Vertex(-1.42500, 0.79800, 0.00000),
    new Vertex(-1.42500, 0.00000, 0.00000),
    new Vertex(-1.50000, -0.84000, 0.15000),
    new Vertex(-0.84000, -1.50000, 0.15000),
    new Vertex(0.00000, -1.50000, 0.15000),
    new Vertex(-1.50000, -0.84000, 0.07500),
    new Vertex(-0.84000, -1.50000, 0.07500),
    new Vertex(0.00000, -1.50000, 0.07500),
    new Vertex(-1.42500, -0.79800, 0.00000),
    new Vertex(-0.79800, -1.42500, 0.00000),
    new Vertex(0.00000, -1.42500, 0.00000),
    new Vertex(0.84000, -1.50000, 0.15000),
    new Vertex(1.50000, -0.84000, 0.15000),
    new Vertex(0.84000, -1.50000, 0.07500),
    new Vertex(1.50000, -0.84000, 0.07500),
    new Vertex(0.79800, -1.42500, 0.00000),
    new Vertex(1.42500, -0.79800, 0.00000)
  ).map(_ * r)
  override val edges: List[(Int, Int)] = List()
}