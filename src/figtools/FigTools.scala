package figtools
import java.util.concurrent.Callable

import org.tsers.zeison.Zeison
import better.files._
import caseapp._
import caseapp.core.help.WithHelp
import figtools.Commands._
import ij.ImagePlus
import net.imagej.legacy.LegacyService
import net.imagej.patcher.LegacyInjector
import org.json4s.native.Serialization.writePretty
import scribe.{Level, Logger}
import net.imagej.ImageJ

object FigTools extends CommandApp[Main] {
  LegacyInjector.preinit()
  val imagej = new ImageJ()
  imagej.context.service(classOf[LegacyService])

  implicit val formats = org.json4s.DefaultFormats + new Box.BoxSerializer
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

  override def appName: String = "FigTools"
  override def appVersion: String = "0.1.0"
  override def progName: String = "figtools"

  val logger = Logger(getClass.getSimpleName)
  val edgeDetectors = Map(
    "susan"->EdgeDetectors.Susan,
    "imagej"->EdgeDetectors.ImageJ)
  var edgeDetector: String = _
  var pdfExportResolution: Int = _

  override def helpAsked(): Nothing = {
    print(beforeCommandMessages.help)
    println(s"Available commands: ${commands.mkString(", ")}\n")
    val commandDescriptions = Map(
      "get"->"Get metadata for FigShare IDs",
      "list"->"List FigShare IDs",
      "search"->"Search for FigShare IDs",
      "download"->"Download figures from figtools into the current directory",
      "download-all"->"Download *ALL* figures from figtools into the current directory.",
      "analyze"->"Recursively analyze and segment a directory full of publication images.")
    for (cmd <- commandsMessages.messages.map{_._1}) {
      commandDescriptions.get(cmd).foreach(d=>println(s"$d\n${"-" * d.length}"))
      println(commandsMessages.messagesMap(cmd).helpMessage(beforeCommandMessages.progName, cmd))
    }
    exit(0)
  }
  override def main(args: Array[String]): Unit = { commandParser.withHelp.detailedParse(args)(beforeCommandParser.withHelp) match {
      case Left(err) => error(err)
      case Right((WithHelp(usage, help, d), dArgs, optCmd)) =>
        if (help || optCmd.isEmpty) helpAsked()
        if (usage) usageAsked()
        d.fold(error, beforeCommand(_, dArgs))
        optCmd.foreach {
          case Left(err) =>
            error(err)
          case Right((c, WithHelp(commandUsage, commandHelp, t), commandArgs)) =>
            if (commandHelp) commandHelpAsked(c)
            if (commandUsage) commandUsageAsked(c)
            t.fold(error, run(_, commandArgs))
        }
    }
    sys.exit(0)
  }

  def run(command: Main, args: RemainingArgs): Unit = {
    command match {
      case get: Get =>
        if (get.common.debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        if (args.remaining.isEmpty) commandHelpAsked("get")
        for {
          id <- args.remaining
          json <- new FigShareApi(get.common.url).get(id)
        } {
          println(Zeison.renderPretty(json))
        }
      case list: List =>
        if (list.common.debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        new FigShareApi(list.common.url).list()
      case search: Search =>
        if (search.common.debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        if (args.remaining.isEmpty) commandHelpAsked("search")
        for (term <- args.remaining) {
          new FigShareApi(search.common.url).search(term)
        }
      case download: Download =>
        if (download.common.debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        if (args.remaining.isEmpty) commandHelpAsked("download")
        for (id <- args.remaining.toList.par) {
          new FigShareApi(download.common.url).download(id, download.outDir)
        }
      case downloadAll: DownloadAll =>
        if (downloadAll.common.debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        new FigShareApi(downloadAll.common.url).downloadAll(downloadAll.outDir)
      case analyze: Analyze =>
        val debug = analyze.common.debug
        if (debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        edgeDetector = analyze.edgeDetector
        pdfExportResolution = analyze.pdfExportResolution
        val results = new AnalyzeImage(
          analyze.edgeDetector,
          analyze.pdfExportResolution,
          analyze.dir.toFile,
          args.remaining,
          debug,
          Some(analyze.common.url),
          File(analyze.dataPath)).analyze()
        if (analyze.json) {
          val json = writePretty(results)
          println(json)
        }
        else {
          println(pprint.apply(results, height=100000))
        }
    }
  }

  object IJ {
    def run(block: =>Any): Unit = {
      val ij1 = LegacyService.getInstance.getIJ1Helper
      val method = ij1.getClass.getDeclaredMethod("runMacroFriendly", classOf[Callable[_]])
      method.setAccessible(true)
      method.invoke(ij1, new Callable[Unit] {
        override def call(): Unit = {
          block
        }
      })
    }
    def run(imp: ImagePlus, command: String, options: String): Unit = {
      run {
        ij.IJ.run(imp, command, options)
      }
    }
  }
}
