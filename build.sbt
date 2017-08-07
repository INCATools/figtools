import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

import sbt._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

val Organization = "figtools"
val Name = "figtools"
val Version = "0.1.0"
val ScalaVersion = "2.12.3"
val DebugPort = 5005

val updatePrependScript = TaskKey[Unit]("update-prepend-script")
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
    resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    libraryDependencies ++= Seq(
      "com.github.scopt" % "scopt_2.12" % "3.6.0",
      "net.imagej" % "ij" % "1.50i",
      "net.sourceforge.tess4j" % "tess4j" % "3.4.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models-english",
      "org.json4s" % "json4s-jackson_2.12" % "3.5.3",
      "com.github.pathikrit" % "better-files_2.12" % "3.0.0",
      "org.scalanlp" % "breeze_2.12" % "0.13.2",
      "org.scalanlp" % "breeze-natives_2.12" % "0.13.2",
      "org.scalanlp" % "breeze-viz_2.12" % "0.13.2",
      "org.jline" % "jline" % "3.3.0"
    ),
    //logLevel in assembly := Level.Debug,
    assemblyOutputPath in assembly := baseDirectory.value / "bin" / name.value,
    assemblyJarName in assembly := name.value,
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false),
    assemblyExcludedJars in assembly := {
      (fullClasspath in assembly).value.filter(_.data.getName != "coursier.jar")
    },
    updatePrependScript := {
      val coursier = (baseDirectory.value / "lib" / "coursier.jar").toString
      val artifactsMap = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
      for (dep <- libraryDependencies.value) {
        val classifiers = dep.explicitArtifacts.flatMap(_.classifier)
        if (classifiers.isEmpty) {
          artifactsMap.addBinding("", dep.toString())
        }
        else {
          for (classifier <- classifiers) {
            artifactsMap.addBinding(classifier, dep.toString())
          }
        }
      }
      val repos = resolvers.value.map(
        _.toString.replaceFirst("""^[^:]*:\s*""", "").split(" ").
          map(x => s"-r $x").mkString(" ")).mkString(" ")

      val jarsSet = mutable.SortedSet[String]()
      val cmds = ArrayBuffer[String]()
      for (classifier <- artifactsMap.keys) {
        val artifacts = artifactsMap(classifier).mkString(" ")
        val classifierOpt = if (classifier.isEmpty) "" else s"-C $classifier"
        val cmd = s"java -noverify -XX:+UseG1GC -cp $coursier coursier.Bootstrap fetch $repos $artifacts $classifierOpt"
        println(s"running cmd: $cmd")
        jarsSet ++= Process(cmd).lines_!.
          map(x => x.replaceFirst(s"""^${Pattern.quote(System.getProperty("user.home"))}/(.*)$$""","""\$HOME/$1"""))
        cmds += s"""java -noverify -XX:+UseG1GC -cp "$$0" coursier.Bootstrap fetch $repos $artifacts $classifierOpt"""
      }
      val jars = jarsSet.mkString(" ")

      val prependShellScript =
        s"""#!/usr/bin/env bash
jars="$jars"
for jar in $$jars; do
  if [[ ! -e $$jar ]]; then
    ${cmds.mkString("\n    ")}
    break
  fi
done
exec java $${DEBUG+-agentlib:jdwp=transport=dt_socket,server=y,address=$DebugPort,suspend=n} -noverify -XX:+UseG1GC $$JAVA_OPTS -cp "$$0:$${jars// /:}" "${(mainClass in assembly).value.get}" "$$@"
"""
      Files.write(Paths.get((baseDirectory.value / "target" / s"${(mainClass in assembly).value.get}.prependShellScript.sh").toString),
        prependShellScript.getBytes(StandardCharsets.UTF_8))
    },
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = {
      val scriptFile = baseDirectory.value/"target"/s"${(mainClass in assembly).value.get}.prependShellScript.sh"
      val prepend = scala.io.Source.fromFile(scriptFile.toString).mkString
      Some(List(prepend))
    })
  )
