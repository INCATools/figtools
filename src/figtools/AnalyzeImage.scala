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
import figtools.ImageSegmenter.ImageSegment
import ij.{IJ, ImageJ, ImagePlus, WindowManager}
import ij.io.Opener
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import java.util

import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.{Tesseract, Word}
import org.tsers.zeison.Zeison
import ImageLog.log
import ij.gui.Roi
import ij.plugin.frame.RoiManager

import scala.collection.mutable
import archery.{Box, Entry, RTree}

import scala.annotation.tailrec

class AnalyzeImage(edgeDetector: String = "imagej", pdfExportResolution: Int = 300) {
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)
  val logger = Logger(getClass.getSimpleName)

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
          val hasCaptions = captionGroups.
            flatMap { cg => cg.captions }.
            flatMap { cs => cs.label }.
            map {
              _.toUpperCase
            }.toSet
          if (hasCaptions.size <= 1) {
            logger.warn(s"File $imageFile only has a single caption, no need to segment the image.")
            break
          }

          val imageFileName = imageFiles.head.toString
          IJ.redirectErrorMessages(true)
          logger.info(s"Opening image file $imageFileName")
          val imp = new Opener().openImage(imageFileName)
          if (imp == null) {
            logger.warn(s"Could not open image file $imageFile, skipping")
            break
          }
          log(imp, "[FigTools] original image")

          case class SegmentDescription(label: String, word: Option[Word], segIndex: Int)
          val segmentDescriptions = mutable.Map[Int, ArrayBuffer[SegmentDescription]]()

          val segments = ImageSegmenter.segment(imp)
          log(imp, "[FigTools] split into segments",
            segments.zipWithIndex.map { case (s, i) => s"seg${i + 1}" -> s.box.toRoi }: _*)
          for ((segment, i) <- segments.zipWithIndex) {
            val cropped = imp.duplicate()
            cropped.setRoi(segment.box.toRoi)
            IJ.run(cropped, "Crop", "")
            //log(cropped, s"[FigTools] cropped segment seg${i+1}")
            // use tesseract OCR
            val instance = new Tesseract()
            val bi = cropped.getBufferedImage
            //log(new ImagePlus(cropped.getTitle,bi), s"[FigTools] bufferedImage seg${i+1}")
            val words = instance.getWords(bi, TessPageIteratorLevel.RIL_WORD).asScala.
              sortBy(x => (-(x.getBoundingBox.width * x.getBoundingBox.height), -x.getConfidence))
            log(new ImagePlus(cropped.getTitle, bi),
              s"[FigTools] Run tesseract OCR on seg${i + 1} (segment only)",
              words.map { w =>
                w.getText -> new Roi(
                  w.getBoundingBox.x,
                  w.getBoundingBox.y,
                  w.getBoundingBox.width,
                  w.getBoundingBox.height)
              }: _*)
            logger.info(s"seg${i + 1} words: ${pprint.apply(words, height = 9999999)}")
            log(imp, s"[FigTools] Run tesseract OCR on seg${i + 1} (overview)",
              Seq(s"seg${i + 1}" -> segment.box.toRoi) ++
                words.map { w =>
                  w.getText -> new Roi(
                    w.getBoundingBox.x + segment.box.x,
                    w.getBoundingBox.y + segment.box.y,
                    w.getBoundingBox.width,
                    w.getBoundingBox.height)
                }: _*)
            for (word <- words) {
              val box = word.getBoundingBox
              val confidence = word.getConfidence
              val text = word.getText
              val captionGroups = CaptionSegmenter.segmentCaption(text)
              logger.info(s"Tesseract OCR seg${i + 1} text: (box: ${pp((box.x, box.y, box.width, box.height))}, confidence: ${pp(confidence)}='${pp(text)}'")
              for {
                captionGroup <- captionGroups
                caption <- captionGroup.captions
                label <- caption.label
              } {
                val ucLabel = label.toUpperCase
                if (hasCaptions.contains(ucLabel)) {
                  logger.info(s"Assigning label $label to segment seg${i + 1}")
                  segmentDescriptions.getOrElseUpdate(i, ArrayBuffer()) +=
                    SegmentDescription(ucLabel, Some(word), i)
                }
              }
            }
          }
          logger.info(s"segmentDescriptions=${pp(segmentDescriptions)}")

          // 2. discover highest scoring layout order:
          //     lrtb tblr rltb tbrl lrbt btlr rlbt btrl
          // TODO: 8 zigzag patterns
          val xmax = segments.map { _.box.x2 }.max
          val ymax = segments.map { _.box.y2 }.max
          val orderedCaptions = captionGroups.
            flatMap { cg => cg.captions }.
            flatMap { cs => cs.label }.
            map { _.toUpperCase }.
            zip(captionGroups.
              flatMap { cg => cg.captions }.
              flatMap { cs => cs.index }).
            sortBy { case (_, i) => i }.
            map { _._1 }
          val bestFixedSegDescrs = (for {
            (ordered, i) <- Seq(
              orderSegments(segments),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.y, s.box.x, s.box.y2, s.box.x2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(xmax - s.box.x, s.box.y, xmax - s.box.x2, s.box.y2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.y, xmax - s.box.x, s.box.y2, xmax - s.box.x2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.x, ymax - s.box.y, s.box.x2, ymax - s.box.y2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(ymax - s.box.y, s.box.x, ymax - s.box.y2, s.box.x2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(xmax - s.box.x, ymax - s.box.y, xmax - s.box.x2, ymax - s.box.y2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(ymax - s.box.y, xmax - s.box.x, ymax - s.box.y2, xmax - s.box.x2)) })).zipWithIndex
          } yield {
            // 3. add labels to unlabeled subpanels that need them
            @tailrec def
            findCaptions(captions: Seq[String],
                         ordered: Seq[Int],
                         segmentDescriptions: collection.Map[Int, Seq[SegmentDescription]]):
            collection.Map[Int, Seq[SegmentDescription]] = {
              val (firstLabeled, rest) = ordered.
                span { i => segmentDescriptions.getOrElse(i, Seq()).nonEmpty }
              val (unlabeled, rest2) = rest.
                span { i => segmentDescriptions.getOrElse(i, Seq()).isEmpty }
              val (lastLabeled, _) = rest2.
                span { i => segmentDescriptions.getOrElse(i, Seq()).nonEmpty }
              val updatedSegDescrs = if (unlabeled.nonEmpty) {
                val firstLabel = firstLabeled.lastOption.
                  map { i => segmentDescriptions(i).map { s => s.label }.max }.getOrElse(captions.head)
                val lastLabel = lastLabeled.headOption.
                  map { i => segmentDescriptions(i).map { s => s.label }.min }.getOrElse(captions.last)

                val foundCaptions = segmentDescriptions.values.flatten.map {
                  _.label
                }.toSet
                val firstLabelIdx = captions.indexWhere(_ == firstLabel)
                val lastLabelIdx = captions.indexWhere(_ == lastLabel)
                val missingCaptions =
                  (if (firstLabelIdx > 0 && lastLabelIdx > 0 && firstLabelIdx < lastLabelIdx)
                    captions.span { _ != firstLabel }._2.span { _ != lastLabel }._1.drop(1)
                  else captions.span { _ != lastLabel }._2.span { _ != firstLabel }._1.drop(1)).
                    filter { l => !foundCaptions.contains(l) }

                segmentDescriptions ++ unlabeled.zipWithIndex.flatMap { case (ui, uii) =>
                  val mc = missingCaptions.slice(
                    (uii.toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt,
                    ((uii + 1).toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt)
                  if (mc.nonEmpty)
                    Seq(ui -> mc.map { c => SegmentDescription(c, None, ui) })
                  else Seq()
                }.toMap
              } else segmentDescriptions
              if (rest2.nonEmpty) findCaptions(captions, rest2, updatedSegDescrs) else updatedSegDescrs
            }
            val updatedSegDescs = findCaptions(orderedCaptions, ordered, segmentDescriptions)

            val captionOrder = orderedCaptions.zipWithIndex.toMap
            val segDescrOrder = updatedSegDescs.toSeq.
              sortBy { _._1 }.
              flatMap { _._2 }.
              map { sd => captionOrder(sd.label) }
            var score = -i
            for ((sdo, i) <- segDescrOrder.zipWithIndex) {
              if (i < segDescrOrder.length - 1) {
                val next = segDescrOrder(i + 1)
                if (next - sdo == 1) score += 1000
                else if (next - sdo > 0) score += 100
              }
            }
            (updatedSegDescs, score)
          }).sortBy { -_._2 }.map { _._1 }.headOption.getOrElse(segmentDescriptions)
          logger.info(s"bestFixedSegDescrs=${pp(bestFixedSegDescrs)}")

          // 4. merge remaining unlabeled subpanels to nearest existing subpanels
          @tailrec def mergeSegments(segments: Seq[(ImageSegmenter.Box[Int], Set[Int])])
          : Seq[(ImageSegmenter.Box[Int], Set[Int])] = {
            val unlabeled = segments.zipWithIndex.find { case ((_, ss), _) =>
              ss.forall { si => bestFixedSegDescrs.getOrElse(si, Seq()).isEmpty }
            }
            unlabeled match {
              case Some(((box, ss), i)) =>
                val nearest = segments.zipWithIndex.
                  filter { case (_, ni) => i != ni }.
                  sortBy { case (s, _) => GappedImageSegmenter.rectDistance(box, s._1) }.
                  headOption
                nearest match {
                  case Some(((nbox, nss), ni)) =>
                    mergeSegments(
                      segments.zipWithIndex.
                        filter { case (_, mi) => mi != i && mi != ni }.
                        map { _._1 } ++
                        Seq((
                          ImageSegmenter.Box(
                            math.min(box.x, nbox.x),
                            math.min(box.y, nbox.y),
                            math.max(box.x2, nbox.x2),
                            math.max(box.y2, nbox.y2)),
                          ss ++ nss)))
                  case None => segments
                }
              case None => segments
            }
          }
          val mergedSegments = mergeSegments(segments.zipWithIndex.map { case (s, i) => (s.box, Set(i)) })
          logger.info(s"mergedSegments=${pp(mergedSegments)}")

          // show
          val captionLabels = ArrayBuffer[(String, Roi)]()
          for {
            (box, ms) <- mergedSegments
            i <- ms
            descs <- bestFixedSegDescrs.get(i)
            desc <- descs
          } {
            val segment = segments(i)
            val roi = box.toRoi
            captionLabels += desc.label->roi
            for {w <- desc.word} {
              captionLabels += w.getText->new Roi(
                w.getBoundingBox.x+segment.box.x,
                w.getBoundingBox.y+segment.box.y,
                w.getBoundingBox.width,
                w.getBoundingBox.height)
            }
          }
          log(imp, "[FigTools] Show caption labels", captionLabels: _*)
          logger.info(s"captionGroups=${pp(captionGroups)}")
          logger.info(s"segmentDescription=${
            pp(bestFixedSegDescrs.map{case (_,d)=>
              d.map{d=>(s"seg${d.segIndex+1}",d.label,d.word)}.mkString("\n")
            })
          }")

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

  // 1. group segments into rows/columns
  def orderSegments(segments: Seq[ImageSegment]): Seq[Int] = {
    val rtree = RTree(segments.zipWithIndex.map { case (s, i) =>
      Entry(Box(s.box.x, s.box.y, s.box.x2, s.box.y2), (s, i))
    }: _*)
    val ordered = new util.LinkedHashSet[Int]().asScala
    while (ordered.size < segments.size) {
      for (first <- segments.zipWithIndex.
        filter { s => !ordered.contains(s._2) }.
        sortBy { s => (s._1.box.y, s._1.box.x) }.headOption)
      {
        ordered += first._2
        for {
          next <- rtree.search(Box(
            first._1.box.x2,
            (first._1.box.y + 0.1 * (first._1.box.y2 - first._1.box.y + 1)).asInstanceOf[Float],
            first._1.box.x2,
            (first._1.box.y2 - 0.1 * (first._1.box.y2 - first._1.box.y + 1)).asInstanceOf[Float])).
          sortWith { case (a, b) =>
            GappedImageSegmenter.rectDistance(
              ImageSegmenter.Box(
                a.value._1.box.x,
                a.value._1.box.y,
                a.value._1.box.x2,
                a.value._1.box.y2),
              ImageSegmenter.Box(
                b.value._1.box.x,
                b.value._1.box.y,
                b.value._1.box.x2,
                b.value._1.box.y2)) < 0
            }.headOption}
        {
          ordered += next.value._2
        }
      }
    }
    ordered.toSeq
  }
}
