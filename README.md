# Scala 3D Render Engine
if god is merciful then why does this exist

## How to use
[Install SBT](https://www.scala-sbt.org/download.html). If you're using IntelliJ and have the Scala extension setup, it
should just auto-import the `build.sbt` and everything Just Works™

once that's done enter your project directory in a command line and run:

```shell
sbt run
```

~~Currently, all configuration is done in the `Projector.scala` script. I want to change this later.~~
Currently this does not work. Good luck.

## Future Improvements
 - Parallelization! (done I guess)
 - Only render shapes within the camera's view (somehow)
 - Configuration beyond editing Scala files directly (which will hopefully allow us to also change things without
   needing to recompile every time we change something)
 - Organize code better
 - Better display than just the command line
 - GPU rendering (somehow)! (how the \*\*\*\* does LWJGL)
 - Actual user interface!
 - Auto map lines somehow?
 - eliminate all ~~heretics~~ mutability and side effects
 - add testing (in progress)

idk I mostly BSed the math using wikipedia and a ton of caffeine, if anyone knows how to actually write good code feel
free to fork this abomination