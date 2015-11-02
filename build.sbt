name := "SGraph"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1"

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.4"

testFrameworks += new TestFramework("utest.runner.Framework")
    