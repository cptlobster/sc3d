# Scala 3D Render Engine
if god is merciful then why does this exist

## How to use
[Install SBT](https://www.scala-sbt.org/download.html) or use your IDE (IntelliJ will manage SBT for you when you import
the project, just make sure you download the Scala plugin)

once that's done enter your project directory in a command line and run:

```shell
sbt run
```

~~Currently, all configuration is done in the `Projector.scala` script. I want to change this later.~~

## Future Improvements
 - Parallelization! (done)
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