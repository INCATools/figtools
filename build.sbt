import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern
import scala.xml.XML

import sbt._
import scala.sys.process._

val Organization = "figtools"
val Name = "figtools"
val Version = "0.1.0"
val ScalaVersion = "2.12.5"
val DebugPort = 5005

lazy val updatePrependScript = TaskKey[String]("updatePrependScript")
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
    javaOptions in Test += "-Djava.awt.headless=true",
    javaOptions in Test += "-Dapple.awt.UIElement=true",
    Test / fork := true,
    Test / envVars := Map("TESSDATA_PREFIX" -> Seq(
      "/usr/local/opt/tesseract/share",
      "/opt/tesseract/share",
      "/usr/share",
      "/usr/share/tesseract-ocr",
      "/usr/local/share").
      find{d=>new java.io.File(s"$d/tessdata").isDirectory}.getOrElse("")),
    resourceDirectory in Compile := baseDirectory.value / "resources",
    resourceDirectory in Test := baseDirectory.value / "test/resources",
    mainClass in Compile := Some("figtools.FigTools"),
    resolvers += "imagej" at "http://maven.imagej.net/content/repositories/thirdparty/",
    resolvers += "imagej public" at "http://maven.imagej.net/content/groups/public/",
    resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    //resolvers += "openimaj-maven" at "http://maven.openimaj.org/",
    //resolvers += "openimaj-snapshots-maven" at "http://snapshots.openimaj.org/",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq(
      //"edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models-english",
      //"edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
      //"fr.inra.ijpb" % "MorphoLibJ_" % "1.3.4",
      //"org.openimaj" % "image-processing" % "1.3.6",
      //"org.tensorflow" % "tensorflow" % "1.2.1",
      //"sc.fiji" % "imagescience" % "3.0.0",
      "com.beachape" %% "enumeratum" % "1.5.12",
      "com.github.alexarchambault" %% "case-app" % "2.0.0-M3",
      "com.github.davidmoten" % "rtree" % "0.8-RC10",
      "com.github.pathikrit" %% "better-files" % "3.0.0",
      "com.lihaoyi" %% "fastparse" % "1.0.0",
      "com.lihaoyi" %% "pprint" % "0.5.3",
      "com.lihaoyi" %% "requests" % "0.1.4",
      "com.lihaoyi" %% "scalatags" % "0.6.7",
      "com.outr" %% "scribe" % "2.6.0",
      "de.sciss" %% "equal" % "0.1.2",
      "io.reactivex" %% "rxscala" % "0.26.5",
      "net.imagej" % "imagej" % "2.0.0-rc-68",
      "net.imagej" % "imagej-legacy" % "0.33.3",
      "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.22",
      "net.sourceforge.tess4j" % "tess4j" % "3.4.0",
      "org.apache.pdfbox" % "pdfbox" % "2.0.11",
      "org.apache.pdfbox" % "pdfbox-tools" % "2.0.11",
      "org.javassist" % "javassist" % "3.23.1-GA",
      "org.jline" % "jline" % "3.9.0",
      "org.json4s" %% "json4s-native" % "3.2.11",
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.tsers.zeison" %% "zeison" % "0.8.0-SNAPSHOT",
      "com.lihaoyi" %% "cask" % "0.1.9",
    ),
    logBuffered in Test := false,
    artifactPath in (Compile, packageBin) := {
      baseDirectory.value / "bin" / name.value
    },
    updatePrependScript := {
      val ivyReportFile = (ivyReport in Compile).value
      val xml = XML.loadFile(ivyReportFile)
      val (deps, depsScript) = (for {
        artifact <- (xml\\"ivy-report"\\"dependencies"\"module"\"revision"\"artifacts"\"artifact").filter{a=> !(a\"@location").text.matches(".*/javassist-3[.]12[.]1[.]GA[.]jar")}
        location = (artifact\"@location").text.
          replaceFirst(
            s"""^${Pattern.quote(System.getProperty("user.home"))}""",
            "\\$HOME")
        originLocation = (artifact \ "origin-location" \ "@location").text
      } yield {(location,s"""test ! -e "$location" && mkdir -p "${new File(location).getParent}" && (set -x; curl -Sso "$location" '$originLocation')""")}).unzip
      val ij1Patcher = (xml\\"ivy-report"\\"dependencies"\"module"\"revision"\"artifacts"\"artifact").
        filter{a=>(a\"@location").text.matches(".*/ij1-patcher-.*[.]jar")}.
        map{a=>(a\"@location").text.
          replaceFirst(
            s"""^${Pattern.quote(System.getProperty("user.home"))}""",
            "\\$HOME")}.
        headOption.getOrElse("")
      val prependShellScript =
        s"""#!/usr/bin/env bash
${"""
TESSDATA_PREFIX=${TESSDATA_PREFIX-$(for d in /usr/local/opt/tesseract/share /usr/share /usr/share/tesseract-ocr /usr/local/share /opt/tesseract/share; do test -d "$d/tessdata" && echo "$d" && break; done)}
PCTMEMORY=${PCTMEMORY-75}
MEMORY=${MEMORY-$(m=$(sysctl -n hw.memsize 2>/dev/null || free -b|perl -0777 -ne 'print [/^Mem:\s+([0-9]+)/ms]->[0]' 2>/dev/null ||true); [[ -n $m ]] && echo $(( m * $PCTMEMORY / 100 / 1048576 ))m)}"""}
${depsScript.mkString("\n")}
export -n DEBUG SHOW AGENT
TF_CPP_MIN_LOG_LEVEL=3 exec java $${DEBUG+ -agentlib:jdwp=transport=dt_socket,server=y,address=$DebugPort,suspend=n} -noverify -XX:+UseG1GC "-Xmx$$MEMORY" $$JAVA_OPTS $${SHOW- -Djava.awt.headless=true -Dapple.awt.UIElement=true} ${if (ij1Patcher.nonEmpty) s"""$${AGENT+ -javaagent:$ij1Patcher}""" else ""} -cp "$$0:${deps.mkString(":")}" "${(mainClass in Compile).value.get}" "$$@"
"""
      val prependScript = (baseDirectory.value / "target" / s"${(mainClass in Compile).value.get}.prependShellScript.sh").toString
      Files.write(Paths.get(prependScript), prependShellScript.getBytes(StandardCharsets.UTF_8))
      prependScript
    },
    packageBin in Compile := {
      val original = (packageBin in Compile).value
      //val prependScript = updatePrependScript.value
      val prependScript = (baseDirectory.value / "target" / s"${(mainClass in Compile).value.get}.prependShellScript.sh").toString
      Seq("bash","-c",s"""cat '$prependScript' <(sed -ne '/^PK/,$$p' '$original') >"$original.$$$$" && mv "$original.$$$$" '$original' && chmod +x '$original'""").!
      original
    }
  )
