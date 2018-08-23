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
import ij.{IJ, ImageJ, WindowManager}
import ij.io.Opener
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import java.util

import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.{Tesseract, Word}
import org.tsers.zeison.Zeison
import ImageLog.log
import com.github.davidmoten.rtree.geometry.{Geometries, Rectangle}
import com.github.davidmoten.rtree.{Entries, RTree}
import ij.gui.Roi
import ij.plugin.frame.RoiManager

import scala.collection.{SortedSet, mutable}
import scala.annotation.tailrec
import de.sciss.equal.Implicits._

class AnalyzeImage
( edgeDetector: String = "imagej",
  pdfExportResolution: Int = 300,
  dir: File = file".",
  ids: Seq[String])
{
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)
  val logger = Logger(getClass.getSimpleName)

  case class SegmentDescription(label: String, labelIndex: Int, word: Option[Word], segIndex: Int)

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

    val iter = if (ids.nonEmpty)
      ids.iterator.flatMap{i=>(dir / i).listRecursively}
    else dir.listRecursively
    for (datapackage <- iter.filter(_.name === "datapackage.json")) {
      val id = datapackage.parent.toString
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
            if (imageFile.extension.isDefined && imageFile.extension.get.toLowerCase === ".pdf") {
              imageFiles.clear()
              val cmd = Seq("convert", "-density", pdfExportResolution.toString, imageFile.toString, s"$imageFile.png")
              val status = cmd.!
              if (status !== 0) {
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

          // TODO: handle checking multiple caption groups, not just the highest-scoring one
          val captionGroups = CaptionSegmenter.segmentCaption(description_nohtml).take(1)
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
          if (imp === null) {
            logger.warn(s"Could not open image file $imageFile, skipping")
            break
          }
          imp.setTitle(s"$id: ${imp.getTitle}")
          log(imp, "[AnalyzeImage] original image")


          implicit def sdOrder: Ordering[SegmentDescription] =
            Ordering.by(sd => (sd.segIndex, sd.labelIndex))

          val segmentDescriptions = mutable.Map[Int, mutable.SortedSet[SegmentDescription]]()

          val segments = ImageSegmenter.segment(imp)
          log(imp, "[AnalyzeImage] split into segments",
            segments.zipWithIndex.map { case (s, i) => s"seg$i" -> s.box.toRoi }: _*)
          for ((segment, i) <- segments.zipWithIndex) {
            val cropped = imp.duplicate()
            cropped.setRoi(segment.box.toRoi)
            IJ.run(cropped, "Crop", "")
            //log(cropped, s"[FigTools] cropped segment seg$i")
            // use tesseract OCR
            val instance = new Tesseract()
            val bi = cropped.getBufferedImage
            //log(new ImagePlus(cropped.getTitle,bi), s"[FigTools] bufferedImage seg$i")
            val words = instance.getWords(bi, TessPageIteratorLevel.RIL_WORD).asScala.
              sortBy(x => (-(x.getBoundingBox.width * x.getBoundingBox.height), -x.getConfidence))
            // log(new ImagePlus(cropped.getTitle, bi),
            //   s"[AnalyzeImage] Run tesseract OCR on seg$i (segment only)",
            //   words.map { w =>
            //     w.getText -> new Roi(
            //       w.getBoundingBox.x,
            //       w.getBoundingBox.y,
            //       w.getBoundingBox.width,
            //       w.getBoundingBox.height)
            //   }: _*)
            logger.info(s"seg$i words: ${pp(words)}")
            log(imp, s"[FigTools] Run tesseract OCR on seg$i (overview)",
              Seq(s"seg$i" -> segment.box.toRoi) ++
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
              logger.info(s"Tesseract OCR seg$i text: (box: ${pp((box.x, box.y, box.width, box.height))}, confidence: ${pp(confidence)}, text: ${pp(text)}, captionGroups: ${pp(captionGroups)}")
              for {
                captionGroup <- captionGroups
                caption <- captionGroup.captions
                li <- caption.label.indices
              } {
                val label = caption.label(li)
                val index = caption.index(li)
                val ucLabel = label.toUpperCase
                if (hasCaptions.contains(ucLabel)) {
                  logger.info(s"Assigning label $label to segment seg$i}")
                  segmentDescriptions.getOrElseUpdate(i, mutable.SortedSet()) +=
                    SegmentDescription(ucLabel, index, Some(word), i)
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
            sortBy { case (_, i) => i }
          val (bestFixedSegDescrs, bestOrder, bestScore) = (for {
            (ordered, i) <- Seq(
              orderSegments(segments),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.y, s.box.x, s.box.y2, s.box.x2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(xmax - s.box.x2, s.box.y, xmax - s.box.x, s.box.y2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.y, xmax - s.box.x2, s.box.y2, xmax - s.box.x)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(s.box.x, ymax - s.box.y2, s.box.x2, ymax - s.box.y)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(ymax - s.box.y2, s.box.x, ymax - s.box.y, s.box.x2)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(xmax - s.box.x2, ymax - s.box.y2, xmax - s.box.x, ymax - s.box.y)) }),
              orderSegments(segments.map { s => ImageSegment(s.imp,
                ImageSegmenter.Box(ymax - s.box.y2, xmax - s.box.x2, ymax - s.box.y, xmax - s.box.x)) })).zipWithIndex
          } yield {
            val updatedSegDescs = findCaptions(orderedCaptions, ordered, segmentDescriptions)

            val captionOrder = orderedCaptions.map{_._1}.zipWithIndex.toMap
            val segDescrOrder = updatedSegDescs.toSeq.
              sortBy { _._1 }.
              flatMap { _._2 }.
              map { sd => captionOrder(sd.label) }
            var score = -i
            for ((sdo, i) <- segDescrOrder.zipWithIndex) {
              if (i < segDescrOrder.length - 1) {
                val next = segDescrOrder(i + 1)
                if (next - sdo === 1) score += 1000
                else if (next - sdo > 0) score += 100
              }
            }
            (updatedSegDescs, ordered, score)
          }).sortBy { -_._3 }.
            headOption.
            getOrElse((segmentDescriptions, orderSegments(segments), 0))

          logger.info(s"bestFixedSegDescrs=${pp(bestFixedSegDescrs)}")
          log(imp, s"best segment order, score $bestScore",
            bestOrder.zipWithIndex.map{case (s,o)=> s"$o seg$s"->segments(s).box.toRoi}: _*)

          // 4. merge remaining unlabeled subpanels to nearest existing subpanels
          @tailrec def mergeSegments
          (segments: Seq[(ImageSegmenter.Box[Int], Set[Int])]):
          Seq[(ImageSegmenter.Box[Int], Set[Int])] =
          {
            val unlabeled = segments.zipWithIndex.find { case ((_, ss), _) =>
              ss.forall { si => bestFixedSegDescrs.getOrElse(si, Seq()).isEmpty }
            }
            unlabeled match {
              case Some(((box, ss), i)) =>
                val nearest = segments.zipWithIndex.
                  filter { case (_, ni) => i !== ni }.
                  sortBy { case (s, _) => box.toRect.distance(s._1.toRect) }.
                  headOption
                nearest match {
                  case Some(((nbox, nss), ni)) =>
                    mergeSegments(
                      segments.zipWithIndex.
                        filter { case (_, mi) => (mi !== i) && (mi !== ni) }.
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
          logger.info(s"bestFixedSegDescrs=${pp(bestFixedSegDescrs)}")

          logger.info(s"Please close current image window to load next image...")

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

  implicit def sdOrder: Ordering[SegmentDescription] =
    Ordering.by(sd => (sd.segIndex, sd.labelIndex))

  // 3. add labels to unlabeled subpanels that need them
  // also, remove duplicate false positive labels
  def findCaptions
  ( // ordered sequence of (labels, labelIndexes) we expect to find
    captions: Seq[(String, Int)],
    // row/column ordering of segment indexes
    ordered: Seq[Int],
    // the set of labels found in each segment by the OCR, with duplicates
    segmentDescriptions: collection.Map[Int, SortedSet[SegmentDescription]]):
  // return segIndex -> set of SegmentDescription(label, labelIndex, word, segIndex)
  collection.Map[Int, SortedSet[SegmentDescription]] = {
    // span: returns a pair consisting of the longest prefix of this
    // general collection whose elements all satisfy p, and the rest
    // of this general collection.
    val segmentOrder = ordered.zipWithIndex.map{case (i,j)=>i->j}.toMap
    val seenLabel = mutable.Map[String, SegmentDescription]()
    val labelAssignments = mutable.Map[Int, SortedSet[SegmentDescription]]()
    var segments = ordered
    while (segments.nonEmpty) {
      // get the span of labeled segments
      val (firstLabeled, rest) = segments.
        span { i => segmentDescriptions.getOrElse(i, Seq()).nonEmpty }
      // then get the next span of unlabeled segments
      val (unlabeled, rest2) = rest.
        span { i => segmentDescriptions.getOrElse(i, Seq()).isEmpty }
      // then get the next span of labeled segments
      val (lastLabeled, _) = rest2.
        span { i => segmentDescriptions.getOrElse(i, Seq()).nonEmpty }

      // remove any duplicate impostor labels
      for (i <- firstLabeled) {
        val sds = segmentDescriptions.getOrElse(i, Seq())
        for (sd <- sds) {
          seenLabel.get(sd.label) match {
            // is sd a more valid candidate than the existing ssd?
            case Some(ssd) =>
              // 1. ordering - in the same segment group
              if (Ordering.by((s: SegmentDescription)=>(
                segmentOrder(s.segIndex),
                // 2. use OCR confidence
                -s.word.map{_.getConfidence}.getOrElse(0f),
                // 3. if the label is the only label in a segment
                labelAssignments.get(s.segIndex).map{_.size}.getOrElse(0),
              )).compare(sd, ssd) < 0)
              {
                // remove the old label assignment
                labelAssignments += ssd.segIndex->(labelAssignments.getOrElse(ssd.segIndex, SortedSet()) - ssd)
                // add the new label assignment
                labelAssignments += i->(labelAssignments.getOrElse(i, SortedSet()) + sd)
                // overwrite the seenLabel entry
                seenLabel += sd.label->sd
              }
            case None =>
              // add new label assignment and seenLabel entry
              labelAssignments += i->(labelAssignments.getOrElse(i, SortedSet()) + sd)
              seenLabel += sd.label->sd
          }
        }
      }

      // add interpolated missing segment labels
      if (unlabeled.nonEmpty) {
        // get the range to interpolate
        val firstLabel = firstLabeled.lastOption.
          map { i => segmentDescriptions(i).map { s => s.label }.max }.getOrElse(captions.head._1)
        val lastLabel = lastLabeled.headOption.
          map { i => segmentDescriptions(i).map { s => s.label }.min }.getOrElse(captions.last._1)

        val firstLabelIdx = captions.indexWhere(_._1 === firstLabel)
        val lastLabelIdx = captions.indexWhere(_._1 === lastLabel)
        val missingCaptions =
          if (firstLabelIdx > 0 && lastLabelIdx > 0 && firstLabelIdx < lastLabelIdx)
            captions.span { _._1 !== firstLabel }._2.span { _._1 !== lastLabel }._1.drop(1)
          else captions.span { _._1 !== lastLabel }._2.span { _._1 !== firstLabel }._1.drop(1)

        // make missing captions
        val interpolated = unlabeled.zipWithIndex.flatMap { case (ui, uii) =>
          val mc = missingCaptions.slice(
            (uii.toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt,
            ((uii + 1).toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt)
          if (mc.nonEmpty)
            Seq(ui -> SortedSet(mc.map { c => SegmentDescription(c._1, c._2, None, ui) }: _*))
          else Seq()
        }.toMap
        // make sure the missing captions should be used
        // do not overwrite any existing captions
        for {
          (i,sds) <- interpolated
          sd <- sds
        } {
          seenLabel.get(sd.label) match {
            case Some(_) =>
            case None =>
              // add new label assignment and seenLabel entry
              labelAssignments += i->(labelAssignments.getOrElse(i, SortedSet()) + sd)
              seenLabel += sd.label->sd
          }
        }
      }
      segments = rest2
    }
    labelAssignments
  }

  // 1. group segments into rows/columns
  def orderSegments(segments: Seq[ImageSegment]): Seq[Int] = {
    val rtree = RTree.create[(ImageSegment,Int),Rectangle].add(
      segments.zipWithIndex.map{case (s,i)=>Entries.entry((s,i), s.box.toRect)}.asJava)

    val ordered = new util.LinkedHashSet[Int]().asScala
    while (ordered.size < segments.size) {
      val first = segments.zipWithIndex.
        filter { s => !ordered.contains(s._2) }.
        minBy { s => (s._1.box.y, s._1.box.x) }

      ordered += first._2
      val rowSearch = rtree.search(Geometries.rectangle(
          first._1.box.x2,
          (first._1.box.y + 0.1 * (first._1.box.y2 - first._1.box.y + 1)).toFloat,
          first._1.imp.getWidth.toFloat,
          (first._1.box.y2 - 0.1 * (first._1.box.y2 - first._1.box.y + 1)).toFloat)).
        toBlocking.getIterator.asScala.filter{e=>e.value._2 !== first._2}.toSeq
      if (rowSearch.nonEmpty) {
        val nearest = rowSearch.minBy{e=> e.geometry().distance(first._1.box.toRect) }
        ordered += nearest.value._2
      }
    }
    ordered.toSeq
  }
}
