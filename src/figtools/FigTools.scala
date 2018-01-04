package figtools
import java.util.Properties
import java.util.regex.Pattern

import better.files._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scopt.OptionParser

import sys.process._
import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration
import ij.ImageJ
import ij.io.Opener
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.Tesseract


object FigTools {
  val logger = Logger("FigTools")
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  case class Config(mode: String = "",
                    pdfExportResolution: Int = 300)

  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("figtools") {
      override def showUsageOnError = true
      head("figtools", "0.1.0")
      help("help").text("prints this usage text")
      version("version").text("print the program version")
      cmd("analyze").action((_, c) => c.copy(mode = "analyze")).
        text("recursively analyze a nested directory of figures.").
        children(
          opt[Int]("pdf-export-resolution").action((x,c) => c.copy(pdfExportResolution = x)).
            text("Resolution to use when exporting PDFs to images"))
    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        config.mode match {
          case "analyze" => analyze(config)
          case "" => parser.showUsage()
        }
      case None =>
        System.exit(1)
    }
  }

  def treeToJSONList(tree: Tree): JValue = {
    JArray(List(JString(tree.label().toString))) ++
      (if (tree.children().length ==1 && tree.getChild(0).isLeaf)
        JString(tree.getChild(0).value())
      else JArray(tree.children().map(t=>if (t.isLeaf) JString(t.value()) else treeToJSONList(t)).toList))
  }

  def treeToJSONObject(tree: Tree): JValue = {
    JObject(List(tree.label().toString ->
      (if (tree.children().length ==1 && tree.getChild(0).isLeaf)
        JString(tree.getChild(0).value())
      else JArray(tree.children().map(t=>if (t.isLeaf) JString(t.value()) else treeToJSONObject(t)).toList))))
  }

  def analyze(config: Config): Unit = {
    // turn off CoreNLP logging
    RedwoodConfiguration.current.clear.apply()
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    val pipeline = new StanfordCoreNLP(props)

    // start an embedded ImageJ instance
    val imagej = new ImageJ(ImageJ.EMBEDDED)
    imagej.exitWhenQuitting(true)

    val dir = "." / "."
    for (datapackage <- dir.listRecursively.filter(_.name == "datapackage.json")) {
      val json = parse(datapackage.contentAsString)
      val description_nohtml = (json \ "description_nohtml").extract[String]
      logger.info(s"file=$datapackage")
      logger.info(s"description_nohtml=\n$description_nohtml")

      val document = new Annotation(description_nohtml)
      pipeline.annotate(document)
      val sentences = document.get(classOf[SentencesAnnotation])

      for (sentence <- sentences.asScala) {
        logger.info(s"sentence=${sentence.toString}")
        //val tree = sentence.get(classOf[TreeAnnotation])
        //val json = treeToJSONObject(tree)
        //logger.info(s"tree=\n${tree.pennString()}")
        //logger.info(s"json=\n${pretty(json)}")
      }
      logger.info("")

      val resources = (json \ "resources").extract[List[JValue]]
      for (resource <- resources) {
        breakable {
          val name = (resource \ "name").extract[String]
          val imageFile = datapackage.parent / name
          if (!imageFile.toString.matches("""(?i).*\.(png|jpe?g|tiff?|pdf)""")) {
            logger.info(s"Skipping non-image file $imageFile")
            break
          }
          val imageFiles = ArrayBuffer(imageFile)
          if (imageFile.exists) {
            // use the convert command-line tool to convert PDF files to image files
            if (imageFile.extension.isDefined && imageFile.extension.get.toLowerCase == ".pdf") {
              imageFiles.clear()
              val cmd = Seq("convert", "-density", config.pdfExportResolution.toString, imageFile.toString, s"$imageFile.png")
              val status = cmd.!
              if (status != 0) {
                throw new RuntimeException(s"Command $cmd returned exit status $status")
              }
              val pngs = datapackage.parent.glob("*.png")
              val outimages = pngs.filter(f =>
                f.name.toString.matches(raw"""^${Pattern.quote(imageFile.name.toString)}(-[0-9]+)?\.png$$"""))
              imageFiles ++= outimages
            }
          }
          else {
            Console.err.println(s"Could not find file $imageFile")
          }
          if (imageFiles.size > 1) {
            logger.warn(s"File $imageFile contains more than one image, skipping")
            break
          }
          if (imageFiles.isEmpty) {
            logger.info(s"File $imageFile contains no images, skipping")
            break
          }
          val imp = new Opener().openImage(imageFiles.head.toString())
          if (imp == null) {
            logger.warn(s"Could not open image file $imageFile, skipping")
            break
          }
          imp.show()

          // use tesseract OCR
          val instance = new Tesseract()
          val bi = imp.getBufferedImage
          val words = instance.getWords(bi, TessPageIteratorLevel.RIL_WORD).asScala
          for (word <- words) {
            val box = word.getBoundingBox
            val confidence = word.getConfidence
            val text = word.getText
            logger.info(s"Tesseract OCR text: (box: ${pp(box)}, confidence: $confidence)='$text'")
          }

          val captionGroups = SegmentCaption.segmentCaption(description_nohtml)
          logger.info(s"captionGroups=${pp(captionGroups)}")

          while (imp.isVisible) {
            Thread.sleep(200)
          }
        }
      }
    }
  }
}
