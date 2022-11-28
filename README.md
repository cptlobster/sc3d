# Scala 3D Render Engine
if god is merciful then why does this exist

## How to use
[Install SBT](https://www.scala-sbt.org/download.html)

once that's done enter your project directory in a command line and run:

```shell
sbt run
```

Currently, all configuration is done in the `Projector.scala` script. I want to change this later. This is how it's set
up with only the important variables and functions shown:
```scala
class Projector {
  var shp: List[Shape] = List(Cube(4), Cube(2), Pyramid(1)) // append shapes here

  var cam: Camera = Camera()

  cam.pos += new Vertex(0, 0, 15) // mess around with these values to move your camera
  cam.rot += new Vertex(0, 0, 0)
  cam.plane += new Vertex(0, 0, 8)

  ...
}

object Main {
  def rotate_shape(shape: Shape, x: Double, y: Double, z: Double): Shape = { // function that applies the rotation
    shape.rot = new Vertex(shape.rot.x + x, shape.rot.y + y, shape.rot.z + z)
    shape
  }
  
  def vectors: List[(Double, Double, Double)] = List( // rotation per frame for individual shapes
    (0, Pi / 16, -Pi / 32),
    (-Pi / 16, Pi / 32, 0),
    (0, Pi / 64, 0)
  )

  def main(args: Array[String]): Unit = {
    ...
    while (true) {
      rend.display(proj.projection) // this draws shapes
      Thread.sleep(50) // adjust frame times
      proj.shp.zip(vectors).map(p => rotate_shape(p._1, p._2._1, p._2._2, p._2._3)) // this actually applies the rotation
    }
  }
}
```

## Future Improvements
 - Parallelization!
 - Only render shapes within the camera's view (somehow)
 - Configuration beyond editing Scala files directly (which will hopefully allow us to also change things without
   needing to recompile every time we change something)
 - Organize code better
 - Better display than just the command line
 - GPU rendering (somehow)!
 - Actual user interface!
 - Auto map lines somehow?
 - Figure out how to make this hellscape fall more in line with how functional programming works
 - Add ability to add update functions directly to shapes

idk I mostly BSed the math using wikipedia and a ton of caffeine, if anyone knows how to actually write good code feel
free to fork this abomination