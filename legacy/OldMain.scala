package dev.cptlobster.sc3d;
object OldMain {
        def rotate_shape(shape: Shape, x: Double, y: Double, z: Double): Shape = {
                shape.rot += Vertex(x, y, z)
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
