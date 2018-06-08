package figtools
import java.util.Properties
import java.util.regex.Pattern

import better.files._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import scopt.OptionParser

import sys.process._
import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration
import figtools.CaptionSegmenter.CaptionGroup
import figtools.ImageSegmenter.ImageSegment
import ij.{ImageJ, ImagePlus}
import ij.io.Opener
import ij.plugin.frame.RoiManager
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.Tesseract
import org.tsers.zeison.Zeison

import scala.collection.mutable


object FigTools {
  val logger = Logger("FigTools")
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

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

  def analyze(config: Config): Unit = {
    // turn off CoreNLP logging
    RedwoodConfiguration.current.clear.apply()
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    val pipeline = new StanfordCoreNLP(props)

    // start an embedded ImageJ instance
    val imagej = new ImageJ(ImageJ.EMBEDDED)
    imagej.exitWhenQuitting(true)

    val dir = file"."
    for (datapackage <- dir.listRecursively.filter(_.name == "datapackage.json")) {
      val json = Zeison.parse(datapackage.contentAsString)
      val description_nohtml = json.description_nohtml.toStr
      logger.info(s"file=$datapackage")
      logger.info(s"description_nohtml=\n$description_nohtml")

      val document = new Annotation(description_nohtml)
      pipeline.annotate(document)
      val sentences = document.get(classOf[SentencesAnnotation])

      for (sentence <- sentences.asScala) {
        logger.info(s"sentence=${sentence.toString}")
      }
      logger.info("")

      val resources = json.resources.toList
      for (resource <- resources) {
        breakable {
          val name = resource.name.toStr
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

          val captionGroups = CaptionSegmenter.segmentCaption(description_nohtml)
          logger.info(s"captionGroups=${pp(captionGroups)}")
          val hasCaptions = captionGroups.flatMap{cg=>cg.captions}.flatMap{cs=>cs.label}.toSet

          val foundCaptions = mutable.Set[String]()
          val segmentDescription = mutable.Map[ImageSegment, CaptionGroup]()

          val segments = ImageSegmenter.segment(imp)
          for (segment <- segments) {
            imp.getProcessor.setRoi(segment.box.toRoi)
            val cropped = new ImagePlus(imp.getTitle, imp.getProcessor.crop())
            // use tesseract OCR
            val instance = new Tesseract()
            val bi = cropped.getBufferedImage
            val words = instance.getWords(bi, TessPageIteratorLevel.RIL_WORD).asScala.
              sortBy(x=>(-(x.getBoundingBox.width * x.getBoundingBox.height), -x.getConfidence))
            for (word <- words) {
              val box = word.getBoundingBox
              val confidence = word.getConfidence
              val text = word.getText
              val captionGroups = CaptionSegmenter.segmentCaption(text)
              for {
                captionGroup <- captionGroups
                caption <- captionGroup.captions
                label <- caption.label
              } {
                if (hasCaptions.contains(label) && !foundCaptions.contains(label)) {
                  segmentDescription(segment) = captionGroup
                }

              }
              logger.info(s"Tesseract OCR text: (box: ${pp(box)}, confidence: $confidence)='$text'")
            }
          }

          // show
          val rm = RoiManager.getRoiManager
          rm.runCommand(imp, "Show All with labels")
          for ((segment, captionGroup) <- segmentDescription) {
            val label = captionGroup.captions.flatMap{c=>c.label}.distinct.sorted.mkString(" ")
            val roi = segment.box.toRoi
            rm.addRoi(roi)
            val index = RoiManager.getRoiManager.getRoiIndex(roi)
            rm.rename(index, label)
          }
          imp.repaintWindow()

          // wait for user to close the image
          while (imp.isVisible) Thread.sleep(200)
        }
      }
    }
  }
}
