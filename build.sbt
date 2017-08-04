import java.util.regex.Pattern

import sbt._

val Organization = "figtools"
val Name = "figtools"
val Version = "0.1.0"
val ScalaVersion = "2.12.3"

val install = TaskKey[Unit]("install")
lazy val figtools = (project in file(".")).
  settings(
    name := Name,
    organization := Organization,
    version := Version,
    scalaVersion := ScalaVersion,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test/src",
    javaSource in Compile := baseDirectory.value / "src",
    javaSource in Test := baseDirectory.value / "test/src",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    resourceDirectory in Test := baseDirectory.value / "test/resources",
    mainClass in assembly := Some("figtools.FigTools"),
    libraryDependencies ++= Seq(
      "com.github.scopt" % "scopt_2.12" % "3.6.0"
    ),
    //logLevel in assembly := Level.Debug,
    assemblyOutputPath in assembly := baseDirectory.value / "bin" / name.value,
    assemblyJarName in assembly := name.value,
    assemblyExcludedJars in assembly := {
      (fullClasspath in assembly).value.filter(_.data.getName != "coursier.jar")
    },
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = {
      val coursier = (baseDirectory.value / "lib" / "coursier.jar").toString
      val artifacts = (libraryDependencies.value).
        mkString(" ")
      val cmd = s"java -cp $coursier coursier.Bootstrap fetch $artifacts"
      val jars = Process(cmd).lines_!.
        map(x => x.replaceFirst(s"""^${Pattern.quote(System.getProperty("user.home"))}/(.*)$$""","""\$HOME/$1""")).
        mkString(" ")

      Some(List(
s"""#!/usr/bin/env bash
jars="$jars"
for jar in $$jars; do
  if [[ ! -e $$jar ]]; then
    java -noverify -XX:+UseG1GC -cp "$$0" coursier.Bootstrap fetch $artifacts >/dev/null
    break
  fi
done
exec java -noverify -XX:+UseG1GC $$JAVA_OPTS -cp "$$0:$${jars// /:}" "${(mainClass in assembly).value.get}" "$$@"
"""))
    })
  )

