/*
 *   Scala 3D renderer - SBT Build Script
 *   Copyright (C) 2022 Dustin Thomas
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

ThisBuild / version := "0.1.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "sc3d",
    idePackagePrefix := Some("dev.cptlobster.sc3d")
  )

libraryDependencies ++= {
  // LWJGL dependencies go here
  val version = "3.2.3"
  val natives = "natives-windows"
  Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-openal",
    "lwjgl-opengl",
  ).flatMap {
    module =>
      Seq(
        "org.lwjgl" % module % version,
        "org.lwjgl" % module % version classifier natives
      )
  }
}

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15"