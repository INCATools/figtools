package figtools
import com.typesafe.scalalogging.Logger
import org.tsers.zeison.Zeison
import caseapp._
import caseapp.core.help.WithHelp

sealed abstract class Main extends Product with Serializable

case class CommonOptions(
  @HelpMessage("URL of FigShare API")
  @ValueDescription("URL")
  url: String = "http://api.figshare.com/v1")

@HelpMessage("Get metadata for FigShare IDs")
@ArgsName("List of FigShare IDs")
final case class Get(@Recurse common: CommonOptions) extends Main

@HelpMessage("List FigShare IDs")
final case class List(@Recurse common: CommonOptions) extends Main

@HelpMessage("Search for FigShare IDs")
@ArgsName("List of search terms")
final case class Search(@Recurse common: CommonOptions) extends Main

@HelpMessage("Download figures from figtools into the current directory")
@ArgsName("List of FigShare IDs")
final case class Download(
  @Recurse common: CommonOptions,
  @HelpMessage("Output directory")
  @ValueDescription("DIRECTORY")
  outDir: String = "."
) extends Main

@HelpMessage("Download *ALL* figures from figtools into the current directory.")
final case class DownloadAll(
  @Recurse common: CommonOptions,
  @HelpMessage("Output directory")
  @ValueDescription("DIRECTORY")
  outDir: String = "."
) extends Main

@HelpMessage("Recursively analyze and segment a directory full of publication images.")
final case class Analyze(
  @HelpMessage("Resolution to use when exporting PDFs to images")
  @ValueDescription("DPI")
  pdfExportResolution: Int = 300,
  @HelpMessage("Edge detector to use. Possible values: susan imagej")
  @ValueDescription("MODULE")
  edgeDetector: String = "imagej"
) extends Main

object FigTools extends CommandApp[Main] {
  override def appName: String = "FigTools"
  override def appVersion: String = "0.1.0"
  override def progName: String = "figtools"

  val logger = Logger("FigTools")
  val edgeDetectors = Map(
    "susan"->EdgeDetectors.Susan,
    "imagej"->EdgeDetectors.ImageJ)
  var edgeDetector: String = _
  var pdfExportResolution: Int = _

  override def main(args: Array[String]): Unit =
    commandParser.withHelp.detailedParse(args)(beforeCommandParser.withHelp) match {
      case Left(err) => error(err)
      case Right((WithHelp(usage, help, d), dArgs, optCmd)) =>
        if (help) helpAsked()
        if (usage) usageAsked()
        if (optCmd.isEmpty) helpAsked()
        d.fold( error, beforeCommand(_, dArgs) )
        optCmd.foreach {
          case Left(err) =>
            error(err)
          case Right((c, WithHelp(commandUsage, commandHelp, t), commandArgs)) =>
            if (commandHelp) commandHelpAsked(c)
            if (commandUsage) commandUsageAsked(c)
            t.fold( error, run(_, commandArgs) )
        }
    }
  def run(command: Main, args: RemainingArgs): Unit = {
    command match {
      case get: Get =>
        if (args.remaining.isEmpty) commandHelpAsked("get")
        for (id <- args.remaining) {
          val json = new FigShareApi(get.common.url).get(id)
          println(Zeison.renderPretty(json))
        }
      case list: List =>
        new FigShareApi(list.common.url).list()
      case search: Search =>
        if (args.remaining.isEmpty) commandHelpAsked("search")
        for (term <- args.remaining) {
          new FigShareApi(search.common.url).search(term)
        }
      case download: Download =>
        if (args.remaining.isEmpty) commandHelpAsked("download")
        for (id <- args.remaining) {
          new FigShareApi(download.common.url).download(id, download.outDir)
        }
      case downloadAll: DownloadAll =>
        new FigShareApi(downloadAll.common.url).downloadAll(downloadAll.outDir)
      case analyze: Analyze =>
        edgeDetector = analyze.edgeDetector
        pdfExportResolution = analyze.pdfExportResolution
        new AnalyzeImage(analyze.edgeDetector, analyze.pdfExportResolution).analyze()
    }
  }
}
