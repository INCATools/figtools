package figtools
import scribe.Logger
import org.tsers.zeison.Zeison
import better.files._
import caseapp._
import caseapp.core.help.WithHelp
import ij.{Executer, IJ, Macro}
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.writePretty
import scribe.{Level, Logger}

sealed abstract class Main extends Product with Serializable

case class CommonOptions
( @HelpMessage("URL of FigShare API")
  @ValueDescription("URL")
  url: String = "http://api.figshare.com/v1",
  @HelpMessage("Enable debug mode")
  debug: Boolean = false)

@ArgsName("List of FigShare IDs")
final case class Get(@Recurse common: CommonOptions) extends Main

final case class List(@Recurse common: CommonOptions) extends Main

@ArgsName("List of search terms")
final case class Search(@Recurse common: CommonOptions) extends Main

@ArgsName("List of FigShare IDs")
final case class Download
( @Recurse common: CommonOptions,
  @HelpMessage("Output directory")
  @ValueDescription("DIRECTORY")
  outDir: String = "."
) extends Main

final case class DownloadAll
( @Recurse common: CommonOptions,
  @HelpMessage("Output directory")
  @ValueDescription("DIRECTORY")
  outDir: String = "."
) extends Main

@ArgsName("List of FigShare IDs to analyze (optional)")
final case class Analyze
( @Recurse common: CommonOptions,
  @HelpMessage("Directory in which to analyze image files")
  @ValueDescription("DIR")
  dir: String = ".",
  @HelpMessage("Resolution to use when exporting PDFs to images")
  @ValueDescription("DPI")
  pdfExportResolution: Int = 300,
  @HelpMessage("Edge detector to use. Possible values: susan imagej")
  @ValueDescription("MODULE")
  edgeDetector: String = "imagej"
) extends Main

object FigTools extends CommandApp[Main] {
  implicit val formats = Serialization.formats(NoTypeHints)
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
  override def main(args: Array[String]): Unit = {
    commandParser.withHelp.detailedParse(args)(beforeCommandParser.withHelp) match {
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
        if (get.common.debug || sys.env.contains("DEBUG")) {
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
        if (list.common.debug || sys.env.contains("DEBUG")) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        new FigShareApi(list.common.url).list()
      case search: Search =>
        if (search.common.debug || sys.env.contains("DEBUG")) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        if (args.remaining.isEmpty) commandHelpAsked("search")
        for (term <- args.remaining) {
          new FigShareApi(search.common.url).search(term)
        }
      case download: Download =>
        if (download.common.debug || sys.env.contains("DEBUG")) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        if (args.remaining.isEmpty) commandHelpAsked("download")
        for (id <- args.remaining.toList.par) {
          new FigShareApi(download.common.url).download(id, download.outDir)
        }
      case downloadAll: DownloadAll =>
        if (downloadAll.common.debug || sys.env.contains("DEBUG")) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        new FigShareApi(downloadAll.common.url).downloadAll(downloadAll.outDir)
      case analyze: Analyze =>
        val debug = analyze.common.debug || sys.env.contains("DEBUG")
        if (debug) {
          scribe.Logger.root.clearHandlers().clearModifiers().
            withHandler(minimumLevel = Some(Level.Debug)).replace()
        }
        edgeDetector = analyze.edgeDetector
        pdfExportResolution = analyze.pdfExportResolution
        implicit val log = ImageLog(showLog=debug)
        val results = new AnalyzeImage(
          analyze.edgeDetector,
          analyze.pdfExportResolution,
          analyze.dir.toFile,
          args.remaining,
          debug,
          Some(analyze.common.url)).analyze()
        val json = writePretty(results)
        println(json)
    }
  }

  def run(command: String, options: String) {
    val e = new Executer(command)
    e.run()
  }
}
