package figtools

import java.awt.GraphicsEnvironment
import java.util.Properties
import java.util.regex.Pattern

import better.files._
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}

import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration
import figtools.ImageSegmenter.ImageSegment
import net.sourceforge.tess4j.ITessAPI.TessPageIteratorLevel

import util.control.Breaks._
import java.util

import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.{Tesseract, Word}
import org.tsers.zeison.Zeison
import com.github.davidmoten.rtree.geometry.{Geometries, Rectangle}
import com.github.davidmoten.rtree.{Entries, RTree}
import ij.gui.Roi
import ij.plugin.frame.RoiManager

import scala.collection.{SortedSet, mutable}
import de.sciss.equal.Implicits._
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.apache.pdfbox.tools.imageio.ImageIOUtil
import java.io.IOException

import figtools.FigTools.IJ
import ij.WindowManager
import ij.io.Opener
import scribe.{Level, Logger}

class AnalyzeImage
( edgeDetector: String = "imagej",
  pdfExportResolution: Int = 300,
  dir: File = file".",
  ids: Seq[String],
  debug: Boolean = false,
  url: Option[String],
)(implicit log: ImageLog)
{
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)
  var logger = Logger(getClass.getSimpleName).
    withHandler(minimumLevel=Some(if (debug) Level.Debug else Level.Info))

  val Epsilon = 0.001
  val BetterConfidence = 10.0

  case class SegmentDescription
  (label: String,
   labelIndex: Int,
   word: Option[Word],
   segIndex: Int)

  if (!FigTools.edgeDetectors.contains(edgeDetector)) {
    Console.err.println(s"Unknown edge detector module: $edgeDetector")
    sys.exit(1)
  }
  // turn off CoreNLP logging
  RedwoodConfiguration.current.clear.apply()
  val props = new Properties()
  props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
  val pipeline = new StanfordCoreNLP(props)

  case class LabelResult(descriptions: Seq[String], rois: Seq[Roi])
  type LabelResults = collection.Map[String, LabelResult]
  type ImageResults = collection.Map[String, LabelResults]
  type AnalysisResults = collection.Map[String, ImageResults]

  def analyze(): AnalysisResults = {
    val results = new util.LinkedHashMap[String, ImageResults]().asScala
    val iter = if (ids.nonEmpty)
    // download the files if the directory does not exist
      ids.iterator.map { i =>
        val d = dir / i
        if (!d.exists) url match {
          case Some(u) => new FigShareApi(u).download(i, dir.toString)
          case None => new FigShareApi().download(i, dir.toString)
        }
        i
      }
    // find all
    else dir.list.
      filter { i => i.name.matches("[0-9]+") && (i / "datapackage.json").exists }.
      map { _.name }
    for (id <- iter) {
      val imageResults = analyzeImages(id)
      results(id) = imageResults
    }
    results
  }

  def analyzeImages(id: String): ImageResults = {
    val datapackage = dir/id/"datapackage.json"
    val json = Zeison.parse(datapackage.contentAsString)
    val description_nohtml = json.description_nohtml.toStr
    logger.debug(s"file=$datapackage")
    logger.debug(s"description_nohtml=\n$description_nohtml")

    val document = new Annotation(description_nohtml)
    pipeline.annotate(document)
    val sentences = document.get(classOf[SentencesAnnotation])

    for (sentence <- sentences.asScala) {
      logger.debug(s"sentence=${sentence.toString}")
    }
    logger.debug("")

    val results = new util.LinkedHashMap[String, LabelResults].asScala
    val resources = json.resources.toList
    for (resource <- resources) {
      breakable {
        if (!GraphicsEnvironment.isHeadless) WindowManager.closeAllWindows()

        val name = resource.name.toStr
        val imageFile = datapackage.parent / name
        if (!imageFile.toString.matches("""(?i).*\.(png|jpe?g|tiff?|pdf)""")) {
          logger.debug(s"Skipping non-image file $imageFile")
          break
        }
        val imageFiles = ArrayBuffer(imageFile)
        if (imageFile.exists) {
          // use the convert command-line tool to convert PDF files to image files
          if (imageFile.extension.isDefined && imageFile.extension.get.toLowerCase === ".pdf") {
            imageFiles.clear()
            for (document <- PDDocument.load(imageFile.toJava).autoClosed) {
              if (document.getNumberOfPages > 1) {
                logger.warn(s"File $imageFile contains more than one image, skipping")
                break
              }
              try {
                val pdfRenderer = new PDFRenderer(document)
                for (page <- 0 until document.getNumberOfPages) {
                  val bim = pdfRenderer.renderImageWithDPI(page, pdfExportResolution, ImageType.RGB)
                  ImageIOUtil.writeImage(bim, s"$imageFile.png", pdfExportResolution)
                }
              }
              catch {
                case e: IOException =>
                  Console.err.println("Exception while trying to create pdf document - " + e)
              }
            }
            val pngs = datapackage.parent.glob("*.png")
            val outimages = pngs.filter(f =>
              f.name.toString.matches(raw"""^${Pattern.quote(imageFile.name.toString)}(-[0-9]+)?\.png$$"""))
            imageFiles ++= outimages
            if (imageFiles.isEmpty) {
              logger.debug(s"File $imageFile contains no images, skipping")
              break
            }
          }
        }
        else {
          Console.err.println(s"Could not find file $imageFile")
        }

        // TODO: handle checking multiple caption groups, not just the highest-scoring one
        val captionGroups = CaptionSegmenter.segmentCaption(description_nohtml).take(1)
        logger.debug(s"captionGroups=${pp(captionGroups)}")
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
        logger.debug(s"Opening image file $imageFileName")
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
          logger.debug(s"seg$i words: ${pp(words)}")
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
            logger.debug(s"Tesseract OCR seg$i text: (box: ${pp((box.x, box.y, box.width, box.height))}, confidence: ${pp(confidence)}, text: ${pp(text)}, captionGroups: ${pp(captionGroups)}")
            for {
              captionGroup <- captionGroups
              caption <- captionGroup.captions
              li <- caption.label.indices
            } {
              val label = caption.label(li)
              val index = caption.index(li)
              val ucLabel = label.toUpperCase
              if (hasCaptions.contains(ucLabel)) {
                logger.debug(s"Assigning label $label to segment seg$i}")
                segmentDescriptions.getOrElseUpdate(i, mutable.SortedSet()) +=
                  SegmentDescription(ucLabel, index, Some(word), i)
              }
            }
          }
        }
        logger.debug(s"segmentDescriptions=${pp(segmentDescriptions)}")

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
          (ordered, orderedIdx) <- Seq(
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
              ImageSegmenter.Box(ymax - s.box.y2, xmax - s.box.x2, ymax - s.box.y, xmax - s.box.x)) }),
          ).zipWithIndex
        } yield {
          val updatedSegDescs = findCaptions(orderedCaptions, ordered, segmentDescriptions)

          var score = -orderedIdx
          val captionOrder = orderedCaptions.map{_._1}.zipWithIndex.toMap
          for ((_, sds) <- updatedSegDescs.toSeq.sortBy{_._1}) {
            val sdsSeq = sds.toSeq
            // reward segments with only one label to discourage undersegmentation
            if (sdsSeq.size === 1) score += 1000
            for ((sd, sdsIndex) <- sdsSeq.zipWithIndex) {
              val sdo = captionOrder(sd.label)
              if (sdsIndex < sdsSeq.size - 1) {
                val nextSd = sdsSeq(sdsIndex + 1)
                val next = captionOrder(nextSd.label)
                // reward when the next segment's caption is the next in sequence
                if (next - sdo === 1) score += 1000
                // smaller reward when the next segment's caption is in the right direction,
                // even if it skips
                else if (next > sdo) score += 100
                // penalty if the next segment's caption is in the wrong direction
                else if (next < sdo) score -= 1000
              }
            }
          }
          logger.debug(s"orderedIdx=$orderedIdx, updatedSegDescs=${pp(updatedSegDescs)}, ordered=${pp(ordered)}, score=$score")
          (updatedSegDescs, ordered, score)
        }).sortBy { -_._3 }.
          headOption.
          getOrElse((segmentDescriptions, orderSegments(segments), 0))

        logger.debug(s"bestFixedSegDescrs=${pp(bestFixedSegDescrs)}")
        log(imp, s"best segment order, score $bestScore",
          bestOrder.zipWithIndex.map{case (s,o)=> s"$o seg$s"->segments(s).box.toRoi}: _*)

        // 4. merge duplicately labeled segments into a single segment as long as
        // that doesn't cause the merged segment to overlap another segment with a
        // different label
        var rtree = RTree.create[SortedSet[SegmentDescription],Rectangle]()
        for ((s,i) <- segments.zipWithIndex) {
          val sds = bestFixedSegDescrs.getOrElse(i, SortedSet())
          val labels = sds.map{s=> s.label}.toSet

          val sBox = s.box.toRect
          val nearest = rtree.nearest(sBox, Double.MaxValue, Int.MaxValue).
            filter{e=> e.value.map{_.label}.toSet === labels}.
            toBlocking.getIterator.asScala.toSeq.headOption
          nearest match {
            case Some(n) =>
              val box = ImageSegmenter.Box(
                math.min(sBox.x1(), n.geometry().x1()),
                math.min(sBox.y1(), n.geometry().y1()),
                math.max(sBox.x2(), n.geometry().x2()),
                math.max(sBox.y2(), n.geometry().y2()))
              val merged = sds ++ n.value()
              val mergedLabels = merged.map{_.label}.toSet
              val overlapping = rtree.search(box.toRect).toBlocking.getIterator.asScala.
                filter{e=> e.value.map{_.label}.toSet !== mergedLabels}.toSeq.headOption
              if (overlapping.isEmpty) {
                rtree = rtree.delete(Entries.entry(n.value(), n.geometry()), true)
                rtree = rtree.delete(Entries.entry(sds, s.box.toRect), true)
                rtree = rtree.add(Entries.entry(merged, box.toRect))
              }
            case None =>
              rtree = rtree.add(Entries.entry(sds, s.box.toRect))
          }
        }
        val mergedSegments = rtree.entries.toBlocking.getIterator.asScala.toSeq
        logger.debug(s"mergedSegments=${pp(mergedSegments)}")

        // show
        val captionLabels = ArrayBuffer[(String, Roi)]()
        for (entry <- mergedSegments) {
          val rect = entry.geometry()
          val sds = entry.value()
          captionLabels += sds.map{_.label}.mkString(" ")->new Roi(
            rect.x1.toInt,
            rect.y1.toInt,
            rect.x2.toInt,
            rect.y2.toInt)
          for {
            s <- sds
            w <- s.word
          } {
            captionLabels += w.getText->new Roi(
              w.getBoundingBox.x+rect.x1.toInt,
              w.getBoundingBox.y+rect.y1.toInt,
              w.getBoundingBox.width,
              w.getBoundingBox.height)
          }
        }
        log(imp, "[FigTools] Show caption labels", captionLabels: _*)
        logger.debug(s"captionGroups=${pp(captionGroups)}")
        logger.debug(s"bestFixedSegDescrs=${pp(bestFixedSegDescrs)}")
        logger.debug(s"""Please close image window "${imp.getTitle}" to load next image...""")

        // add to results map
        val descriptions = new util.LinkedHashMap[String,ArrayBuffer[String]]().asScala
        for {
          cg <- captionGroups
          cs <- cg.captions
          l <- cs.label
        } yield {
          if (!descriptions.contains(l)) descriptions(l) = ArrayBuffer()
          descriptions(l) += cs.description
        }
        val captions = mutable.Map[String,ArrayBuffer[Roi]]()
        for (entry <- mergedSegments) {
          val rect = entry.geometry()
          val sds = entry.value()
          for (l <- sds.map{_.label}) {
            if (!captions.contains(l)) captions(l) = ArrayBuffer()
            captions(l) += new Roi(rect.x1.toInt, rect.y1.toInt, rect.x2.toInt, rect.y2.toInt)
          }
        }
        results(resource.name.toStr) = (descriptions.keySet ++ captions.keySet).
          map{s=> s->LabelResult(descriptions.getOrElse(s, Seq()), captions.getOrElse(s, Seq()))}.toMap

        // wait for user to close the image
        if (!GraphicsEnvironment.isHeadless) {
          while (WindowManager.getWindowCount > 0) Thread.sleep(200)

          // clean up ROI list
          RoiManager.getRoiManager.reset()
        }

        // collect garbage
        System.gc()
      }
    }
    results
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

    // first, remove any duplicate impostor labels
    var segments = ordered
    while (segments.nonEmpty) {
      // get the span of labeled segments
      val (firstLabeled, rest) = segments.
        span { i => segmentDescriptions.getOrElse(i, Seq()).nonEmpty }
      // then get the next span of unlabeled segments
      val (_, rest2) = rest.
        span { i => segmentDescriptions.getOrElse(i, Seq()).isEmpty }

      logger.debug(s"ordered=${pp(ordered)}, segmentOrder=${pp(segmentOrder)}, labelAssignments=${pp(labelAssignments)}")
      for (i <- firstLabeled) {
        val sds = segmentDescriptions.getOrElse(i, Seq())
        for (sd <- sds) {
          seenLabel.get(sd.label) match {
            // is sd a more valid candidate than the existing ssd?
            case Some(ssd) =>
              logger.debug(s"Comparing sd=${pp(sd)} (segmentOrder=${segmentOrder(sd.segIndex)}) with ssd=${pp(ssd)} (segmentOrder=${segmentOrder(ssd.segIndex)})")
              // if the confidence is much greater
              if (sd.word.map { _.getConfidence }.getOrElse(0f) -
                ssd.word.map { _.getConfidence }.getOrElse(0f) >= BetterConfidence ||
                (math.abs(sd.word.map { _.getConfidence }.getOrElse(0f) -
                  ssd.word.map { _.getConfidence }.getOrElse(0f)) < BetterConfidence) && (
                // or, if the label is the only label in a segment
                labelAssignments.get(sd.segIndex).map { _.size }.getOrElse(0) + 1 <
                labelAssignments.get(ssd.segIndex).map { _.size }.getOrElse(0) ||
                (labelAssignments.get(sd.segIndex).map { _.size }.getOrElse(0) + 1 ===
                  labelAssignments.get(ssd.segIndex).map { _.size }.getOrElse(0) && (
                  // or, if the confidence is greater
                  sd.word.map { _.getConfidence }.getOrElse(0f) -
                    ssd.word.map { _.getConfidence }.getOrElse(0f) >= Epsilon ||
                    (math.abs(sd.word.map { _.getConfidence }.getOrElse(0f) -
                      ssd.word.map { _.getConfidence }.getOrElse(0f)) < Epsilon)))))
              {
                logger.debug(s"Overwriting ssd with sd")
                // remove the old label assignment
                labelAssignments += ssd.segIndex -> (labelAssignments.getOrElse(ssd.segIndex, SortedSet()) - ssd)
                // add the new label assignment
                labelAssignments += i -> (labelAssignments.getOrElse(i, SortedSet()) + sd)
                // overwrite the seenLabel entry
                seenLabel += sd.label -> sd
              }
              else {
                logger.debug(s"NOT overwriting ssd with sd")
              }
            case None =>
              // add new label assignment and seenLabel entry
              labelAssignments += i -> (labelAssignments.getOrElse(i, SortedSet()) + sd)
              seenLabel += sd.label -> sd
          }
        }
      }
      segments = rest2
    }

    // next, fill in any unlabeled segments
    segments = ordered
    while (segments.nonEmpty) {
      // get the span of labeled segments
      val (firstLabeled, rest) = segments.
        span { i => labelAssignments.getOrElse(i, Seq()).nonEmpty }
      // then get the next span of unlabeled segments
      val (unlabeled, rest2) = rest.
        span { i => labelAssignments.getOrElse(i, Seq()).isEmpty }
      // then get the next span of labeled segments
      val (lastLabeled, _) = rest2.
        span { i => labelAssignments.getOrElse(i, Seq()).nonEmpty }

      // add interpolated missing segment labels
      if (unlabeled.nonEmpty && captions.nonEmpty) {
        logger.debug(s"analyzing unlabeled segments ${pp(unlabeled)}")
        // get the range to interpolate
        val firstLabel = firstLabeled.lastOption.
          map { i => labelAssignments(i).map {s=> (s.label, s.labelIndex)}.max}.
          getOrElse(captions.head)
        logger.debug(s"firstLabel=$firstLabel")
        val lastLabel = lastLabeled.headOption.
          map { i => labelAssignments(i).map {s=> (s.label, s.labelIndex)}.min}.
          getOrElse(captions.last)
        logger.debug(s"lastLabel=$lastLabel")

        val firstLabelIdx = captions.indexWhere(_._1 === firstLabel._1)
        logger.debug(s"firstLabelIdx=$firstLabelIdx")
        val lastLabelIdx = captions.indexWhere(_._1 === lastLabel._1)
        logger.debug(s"lastLabelIdx=$lastLabelIdx")
        var missingCaptions =
          if (firstLabelIdx === lastLabelIdx) Seq(firstLabel)
          else if (firstLabelIdx >= 0 && lastLabelIdx >= 0 && firstLabelIdx < lastLabelIdx)
            captions.
              span { _._1 !== firstLabel._1 }._2.
              span { _._1 !== lastLabel._1 }._1
          else captions.
            span { _._1 !== lastLabel._1 }._2.
            span { _._1 !== firstLabel._1 }._1.
            reverse
        if (missingCaptions.size > 1) missingCaptions = missingCaptions.drop(1)
        logger.debug(s"missingCaptions=$missingCaptions")

        // make missing captions
        val interpolated = unlabeled.zipWithIndex.flatMap { case (ui, uii) =>
          val startSlice = math.floor(uii.toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt
          val endSlice = math.ceil((uii + 1).toDouble / unlabeled.size.toDouble * missingCaptions.size.toDouble).toInt
          val mc = missingCaptions.slice(startSlice, endSlice)
          val ret = if (mc.nonEmpty)
            Seq(ui -> SortedSet(mc.map { c => SegmentDescription(c._1, c._2, None, ui) }: _*))
          else Seq()
          logger.debug(s"ui=$ui, uii=$uii, startSlice=$startSlice, endSlice=$endSlice, mc=${pp(mc)}, ret=${pp(ret)}")
          ret
        }.toMap
        logger.debug(s"interpolated=$interpolated")
        // make sure the missing captions should be used
        // do not overwrite any existing captions
        for {
          (i,sds) <- interpolated
          sd <- sds
        } {
          // add new label assignment and seenLabel entry
          logger.debug(s"Adding label ${pp(sd)} to segment $i")
          labelAssignments += i->(labelAssignments.getOrElse(i, SortedSet()) + sd)
          seenLabel += sd.label->sd
        }
      }
      segments = rest2
    }
    labelAssignments
  }

  // 1. group segments into rows/columns
  def orderSegments(segments: Seq[ImageSegment]): Seq[Int] = {
    var rtree = RTree.create[(ImageSegment,Int),Rectangle].add(
      segments.zipWithIndex.map{case (s,i)=>Entries.entry((s,i), s.box.toRect)}.asJava)

    val ordered = new util.LinkedHashSet[Int].asScala
    while (!rtree.isEmpty) {
      val row = rtree.entries.toBlocking.getIterator.asScala.filter{e=>
        rtree.search(Geometries.rectangle(
          e.value._1.box.x,
          math.min(0, e.value._1.box.y-1),
          e.value._1.box.x2,
          e.value._1.box.y-1,
        )).toBlocking.getIterator.asScala.isEmpty
      }.toSeq.sortBy{e=> (e.value._1.box.x, e.value._1.box.y)}
      rtree = rtree.delete(row.asJava, true)
      ordered ++= row.map{e=> e.value._2}
    }
    ordered.toSeq
  }
}
