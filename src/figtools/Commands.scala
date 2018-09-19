package figtools

import caseapp.{ArgsName, HelpMessage, Recurse, ValueDescription}

object Commands {
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
    edgeDetector: String = "imagej",
    @HelpMessage("Data Path for Tesseract OCR")
    @ValueDescription("PATH")
    dataPath: String = sys.env.getOrElse("TESSDATA_PREFIX", "."),
  ) extends Main
}
