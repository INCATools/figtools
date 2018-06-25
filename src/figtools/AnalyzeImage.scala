package figtools

import java.util.Properties
import java.util.regex.Pattern

import better.files._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}

import sys.process._
import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration
import figtools.CaptionSegmenter.CaptionGroup
import figtools.ImageSegmenter.ImageSegment
import ij.{IJ, ImageJ, ImagePlus, WindowManager}
import ij.io.Opener
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.{Tesseract, Word}
import org.tsers.zeison.Zeison
import ImageLog.log
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.ImageProcessor

import scala.collection.mutable

class AnalyzeImage(edgeDetector: String = "imagej", pdfExportResolution: Int = 300) {
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)
  val logger = Logger("EdgeDetectors")

  def analyze() {
    if (!FigTools.edgeDetectors.contains(edgeDetector)) {
      Console.err.println(s"Unknown edge detector module: $edgeDetector")
      sys.exit(1)
    }
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
          WindowManager.closeAllWindows()

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
              val cmd = Seq("convert", "-density", pdfExportResolution.toString, imageFile.toString, s"$imageFile.png")
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

          val captionGroups = CaptionSegmenter.segmentCaption(description_nohtml)
          logger.info(s"captionGroups=${pp(captionGroups)}")
          val hasCaptions = captionGroups.flatMap{cg=>cg.captions}.flatMap{cs=>cs.label}.toSet
          if (hasCaptions.size <= 1) {
            logger.warn(s"File $imageFile only has a single caption, no need to segment the image.")
            break
          }

          val imageFileName =  imageFiles.head.toString
          IJ.redirectErrorMessages(true)
          logger.info(s"Opening image file $imageFileName")
          val imp = new Opener().openImage(imageFileName)
          if (imp == null) {
            logger.warn(s"Could not open image file $imageFile, skipping")
            break
          }
          log(imp, "[FigTools] original image")

          val foundCaptions = mutable.Set[String]()
          val segmentDescription = mutable.Map[ImageSegment, (CaptionGroup, Word)]()

          val segments = ImageSegmenter.segment(imp)
          log(imp, "[FigTools] split into segments",
            segments.zipWithIndex.map{case (s,i)=>s"seg${i+1}"->s.box.toRoi}: _*)
          for ((segment,i) <- segments.zipWithIndex) {
            val cropped = imp.duplicate()
            cropped.setRoi(segment.box.toRoi)
            IJ.run(cropped, "Crop", "")
            //log(cropped, s"[FigTools] cropped segment seg${i+1}")
            // use tesseract OCR
            val instance = new Tesseract()
            val bi = cropped.getBufferedImage
            //log(new ImagePlus(cropped.getTitle,bi), s"[FigTools] bufferedImage seg${i+1}")
            val words = instance.getWords(bi, TessPageIteratorLevel.RIL_WORD).asScala.
              sortBy(x=>(-(x.getBoundingBox.width * x.getBoundingBox.height), -x.getConfidence))
            log(new ImagePlus(cropped.getTitle,bi),
              s"[FigTools] Run tesseract OCR on seg${i+1} (segment only)",
              words.map{w=>w.getText->new Roi(
                w.getBoundingBox.x,
                w.getBoundingBox.y,
                w.getBoundingBox.width,
                w.getBoundingBox.height)}: _*)
            logger.info(s"seg${i+1} words: ${pprint.apply(words, height=9999999)}")
            log(imp, s"[FigTools] Run tesseract OCR on seg${i+1} (overview)",
              Seq(s"seg${i+1}"->segment.box.toRoi) ++
                words.map{w=>
                  w.getText->new Roi(
                    w.getBoundingBox.x+segment.box.x,
                    w.getBoundingBox.y+segment.box.y,
                    w.getBoundingBox.width,
                    w.getBoundingBox.height)}: _*)
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
                  segmentDescription(segment) = (captionGroup, word)
                }
              }
              logger.info(s"Tesseract OCR seg${i+1} text: (box: ${pp((box.x, box.y, box.width, box.height))}, confidence: ${pp(confidence)}='${pp(text)}'")
            }
          }

          // show
          val captionLabels = ArrayBuffer[(String,Roi)]()
          for ((segment, (captionGroup, word)) <- segmentDescription) {
            val label = captionGroup.captions.flatMap{c=>c.label}.distinct.sorted.mkString(" ")
            val roi = segment.box.toRoi
            captionLabels += label->roi
            captionLabels += word.getText->new Roi(
              word.getBoundingBox.x+segment.box.x,
              word.getBoundingBox.y+segment.box.y,
              word.getBoundingBox.width,
              word.getBoundingBox.height)
          }
          log(imp, "[FigTools] Show caption labels", captionLabels: _*)
          logger.info(s"captionGroups=${pp(captionGroups)}")

          // wait for user to close the image
          while (WindowManager.getWindowCount > 0) Thread.sleep(200)

          // clean up ROI list
          RoiManager.getRoiManager.reset()

          // collect garbage
          System.gc()
        }
      }
    }
  }

}
