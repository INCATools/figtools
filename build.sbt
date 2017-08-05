import java.util
import java.util.regex.Pattern

import sbt._

val Organization = "figtools"
val Name = "figtools"
val Version = "0.1.0"
val ScalaVersion = "2.12.3"

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
    resolvers += "imagej" at "http://maven.imagej.net/content/repositories/thirdparty/",
    resolvers += "imagej public" at "http://maven.imagej.net/content/groups/public/",
    libraryDependencies ++= Seq(
      "com.github.scopt" % "scopt_2.12" % "3.6.0",
      "net.imagej" % "ij" % "1.50i",
      "net.sourceforge.tess4j" % "tess4j" % "3.4.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models-english",
      "org.json4s" % "json4s-jackson_2.12" % "3.5.3",
      "com.github.pathikrit" % "better-files_2.12" % "3.0.0"
    ),
    //logLevel in assembly := Level.Debug,
    assemblyOutputPath in assembly := baseDirectory.value / "bin" / name.value,
    assemblyJarName in assembly := name.value,
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false),
    assemblyExcludedJars in assembly := {
      (fullClasspath in assembly).value.filter(_.data.getName != "coursier.jar")
    },
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = {
      val coursier = (baseDirectory.value / "lib" / "coursier.jar").toString
      val artifacts = (libraryDependencies.value).
        map(x => {
          val parts: Array[String] = x.explicitArtifacts.map(x=>x.)
          val artfact = parts.take(3).mkString(":")
          val classifier = parts.drop(3).mkString(":")
          s"$artifact${if (classifier.isEmpty) "" else s" -C $classifier"}"
        }).mkString(" ")

      val cmd = s"java -cp $coursier coursier.Bootstrap fetch $artifacts"
      println(s"running cmd: $cmd")
      val jars = Process(cmd).lines_!.
        map(x => x.replaceFirst(s"""^${Pattern.quote(System.getProperty("user.home"))}/(.*)$$""","""\$HOME/$1""")).
        mkString(" ")

      val repos = resolvers.value.map(
        _.toString.replaceFirst("""^[^:]*:\s*""","").split(" ").
          map(x=>s"-r $x").mkString(" ")).mkString(" ")

      Some(List(
s"""#!/usr/bin/env bash
jars="$jars"
for jar in $$jars; do
  if [[ ! -e $$jar ]]; then
    java -noverify -XX:+UseG1GC -cp "$$0" coursier.Bootstrap fetch $repos $artifacts >/dev/null
    break
  fi
done
exec java -noverify -XX:+UseG1GC $$JAVA_OPTS -cp "$$0:$${jars// /:}" "${(mainClass in assembly).value.get}" "$$@"
"""))
    })
  )

