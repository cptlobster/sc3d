package dev.cptlobster.sc3d;
case class ProjectedShape(points: ParArray[Vertex], edges: ParSeq[(Int, Int)]) {
        private def drawLine(arr: ParArray[ParArray[Boolean]], p1: Vertex, p2: Vertex): ParArray[ParArray[Boolean]] = {
                var rx: Int = p1.x
                var ry: Int = p1.y
                val dx: Double = p2.x - p1.x
                val dy: Double = p2.y - p1.y

                if (dx != 0 && abs(dy / dx) <= 1) {
                        val yi: Double = ry - (dy / dx) * rx
                        def f(xi: Int): Double = (dy / dx) * xi + yi
                        while (rx != p2.x) {
                                val y: Int = round(f(rx)).toInt
                                if ((0 until arr.size).contains(y)) if ((0 until arr(y).size).contains(rx)) arr(y)(rx) = true
                                if (dx >= 0) rx += 1 else rx -= 1
                                }
                        }
                        else if (dy != 0 && abs(dx / dy) < 1) {
                        val xi: Double = rx - (dx / dy) * ry
                        def f(yi: Int): Double = (dx / dy) * yi + xi
                        while (ry != p2.y) {
                                val x: Int = round(f(ry)).toInt
                                if ((0 until arr.size).contains(ry)) if ((0 until arr(ry).size).contains(x)) arr(ry)(x) = true
                                if (dy >= 0) ry += 1 else ry -= 1
                                }
                        }
                arr
                }

        def rasterize(rows: Int, cols: Int, mx: Int, my: Int): ParArray[ParArray[Boolean]] = {
                var arr: ParArray[ParArray[Boolean]] = Array.ofDim[Boolean](rows, cols).par.map(_.par)
                val ints: ParArray[Vertex] = points.map(_.asInstanceOf[Vertex])
                for (i <- ints) {
                        val x: Int = round(i.x)
                        val y: Int = round(i.y)
                        if ((0 until arr.size).contains(y)) if ((0 until arr(y).size).contains(x)) arr(y)(x) = true
                        }
                for (i <- edges) { arr = drawLine(arr, ints(i._1), ints(i._2)) }
                arr
                }
        }
