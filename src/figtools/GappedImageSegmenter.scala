package figtools

import archery.{Entry, RTree}
import com.typesafe.scalalogging.Logger
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer

import scala.collection.mutable.ArrayBuffer
import ImageLog.log
import ij.gui.Roi

import scala.annotation.tailrec

// TODO:
// - fix recover small components Fig_3.tif
// - recover missing panels
//   - look at overlapping missing panels to determine proper spacing
//   - check matching border colors
class GappedImageSegmenter extends ImageSegmenter {
  val logger = Logger(getClass.getSimpleName)
  val BinarizeThreshold = 0.95
  val ParticleThreshold = 20.0
  val largeParticleFilter = 0.9
  val MergeThreshold = 0.1
  val BboxThreshold = 0.2
  val NewBoxPctCutoff = 0.2
  val SegmentAreaCutoff = 0.5
  val ContentDiff = 0.1
  val ContentMin = 0.01

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val imp2 = imp.duplicate()
    //log(imp2, "[GappedImageSegmenter] original image")

    // binarize the image using a threshold of 0.95
    val threshold = (255.0 * BinarizeThreshold).toInt
    val lut = (0 until 256).map{v=>if (v < threshold) 0 else 255.toByte.toInt}.toArray
    imp2.getProcessor.applyTable(lut)
    log(imp2, s"[GappedImageSegmenter] Binarize with $threshold threshold")

    val rt = new ResultsTable
    val particleAnalyzer = new ParticleAnalyzer(
      ParticleAnalyzer.CLEAR_WORKSHEET |
      ParticleAnalyzer.IN_SITU_SHOW,
      Measurements.MIN_MAX |
      Measurements.RECT,
      rt,
      0.0,
      Double.MaxValue)
    if (!particleAnalyzer.analyze(imp2)) throw new RuntimeException(
      "ParticleAnalyzer.analyze() returned false!")
    val particles = (0 until rt.getCounter).map{i=>
      Option(rt.getLabel(i)).getOrElse(s"Roi${i+1}")->
      new Roi(
        rt.getValue("BX", i).toInt,
        rt.getValue("BY", i).toInt,
        rt.getValue("Width", i).toInt,
        rt.getValue("Height", i).toInt)}
    log(imp2, s"[GappedImageSegmenter] Particle Analyzer", particles: _*)

    // Get the objects and iterate through them
    var segs = ArrayBuffer[ImageSegment]()
    val smallSegs = ArrayBuffer[ImageSegment]()
    (0 until rt.getCounter).foreach{i=>
      val bx = rt.getValue("BX", i).toInt
      val by = rt.getValue("BY", i).toInt
      val width = rt.getValue("Width", i).toInt
      val height = rt.getValue("Height", i).toInt

      if (width > imp2.getWidth.toDouble / ParticleThreshold &&
          height > imp2.getHeight.toDouble / ParticleThreshold)
      {
        if ((width*height).toDouble / (imp2.getWidth*imp2.getHeight).toDouble < largeParticleFilter) {
          val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
          val segment = ImageSegment(imp, box)
          segs += segment
        }
      }
      else {
        val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
        val segment = ImageSegment(imp, box)
        smallSegs += segment
      }
    }
    log(imp2,
      s"[GappedImageSegmenter] filter boxes smaller than (${(imp2.getWidth.toDouble / ParticleThreshold).toInt},${(imp2.getHeight.toDouble / ParticleThreshold).toInt})",
      segs.map{_.box.toRoi}: _*)

    @tailrec def mergeOverlappingSegs(segs: Seq[ImageSegment]): Seq[ImageSegment] = {
      val rtree = RTree(segs.map{s=>Entry(s.box.toArchery, s)}: _*)
      val overlapping = segs.flatMap { o =>
        val overlaps = rtree.searchIntersection(o.box.toArchery).filter { overlaps =>
          val intersectionBox = ImageSegmenter.Box(
            math.max(o.box.x, overlaps.value.box.x),
            math.max(o.box.y, overlaps.value.box.y),
            math.min(o.box.x2, overlaps.value.box.x2),
            math.min(o.box.y2, overlaps.value.box.y2))
          val intersectionArea =
            (intersectionBox.x2 - intersectionBox.x) *
              (intersectionBox.y2 - intersectionBox.y)
          val minBoxArea = math.min(
            (o.box.x2 - o.box.x) * (o.box.y2 - o.box.y),
            (overlaps.value.box.x2 - overlaps.value.box.x) * (overlaps.value.box.y2 - overlaps.value.box.y))
          intersectionArea.toDouble / minBoxArea.toDouble > MergeThreshold
        }.filter{_.value != o}
        if (overlaps.nonEmpty) Seq((o,overlaps.head.value)) else Seq()
      }.headOption

      overlapping match {
        case Some((o,overlaps)) =>
          mergeOverlappingSegs(
            segs.filter { s => overlaps != s && o != s } ++
              Seq(ImageSegment(
                o.imp,
                ImageSegmenter.Box(
                  math.min(o.box.x, overlaps.box.x),
                  math.min(o.box.y, overlaps.box.y),
                  math.max(o.box.x2, overlaps.box.x2),
                  math.max(o.box.y2, overlaps.box.y2)))))
        case None => segs
      }
    }
    val mergedSegs = mergeOverlappingSegs(segs)
    segs = ArrayBuffer(mergedSegs: _*)
    logger.info(s"mergedSegs=${pprint.apply(mergedSegs, height=1000)}")
    log(imp2, "[GappedImageSegmenter] merge overlapping segments", mergedSegs.map{_.box.toRoi}: _*)

    // temporarily eliminate small components
    val (larger, smaller) = segs.sortBy({seg=>
        (seg.box.x2 - seg.box.x) *
        (seg.box.y2 - seg.box.y)
      }).lastOption match
    {
      case Some(largest) =>
        val larger = segs.filter(seg=>
          (seg.box.x2 - seg.box.x).toDouble / (largest.box.x2 - largest.box.x).toDouble > BboxThreshold &&
          (seg.box.y2 - seg.box.y).toDouble / (largest.box.y2 - largest.box.y).toDouble > BboxThreshold)
        val smaller = segs.filter(seg=>
          !((seg.box.x2 - seg.box.x).toDouble / (largest.box.x2 - largest.box.x).toDouble > BboxThreshold &&
            (seg.box.y2 - seg.box.y).toDouble / (largest.box.y2 - largest.box.y).toDouble > BboxThreshold))
        (larger, smaller)
      case None =>
        logger.warn(s"No largest bounding box found!")
        (segs, Seq())
    }
    log(imp2, "[GappedImageSegmenter] temporarily eliminate small components", larger.map{s=>s.box.toRoi}: _*)

    // recover missing panels
    val imp3 = imp2.duplicate()
    val largerRtree = RTree(larger.map{s=>Entry(s.box.toArchery, s)}: _*)
    val recoverMissing = larger.flatMap{seg =>
      imp3.setRoi(seg.box.toRoi)
      val segArea = ((seg.box.x2 - seg.box.x) * (seg.box.y2 - seg.box.y)).toDouble
      val segHisto = imp3.getProcessor.getHistogram
      val segContent = segHisto(0).toDouble / segArea
      logger.info(s"segContent=$segContent")

      val up = ImageSegmenter.Box(seg.box.x,seg.box.y-seg.box.height,seg.box.x2,seg.box.y-1)
      imp3.setRoi(up.toRoi)
      val upHisto = imp3.getProcessor.getHistogram
      val upContent = upHisto(0).toDouble / segArea
      logger.info(s"upContent=$upContent")

      val down = ImageSegmenter.Box(seg.box.x,seg.box.y+seg.box.height+1,seg.box.x2,seg.box.y2+seg.box.height)
      imp3.setRoi(down.toRoi)
      val downHisto = imp3.getProcessor.getHistogram
      val downContent = downHisto(0).toDouble / segArea
      logger.info(s"downContent=$downContent")

      val left = ImageSegmenter.Box(seg.box.x-seg.box.width,seg.box.y,seg.box.x-1,seg.box.y2)
      imp3.setRoi(left.toRoi)
      val leftHisto = imp3.getProcessor.getHistogram
      val leftContent = leftHisto(0).toDouble / segArea
      logger.info(s"leftContent=$leftContent")

      val right = ImageSegmenter.Box(seg.box.x+seg.box.width+1,seg.box.y,seg.box.x2+seg.box.width,seg.box.y2)
      imp3.setRoi(right.toRoi)
      val rightHisto = imp3.getProcessor.getHistogram
      val rightContent = rightHisto(0).toDouble / segArea
      logger.info(s"rightContent=$rightContent")

      (if (math.abs(segContent-upContent) < ContentDiff &&
        upContent > ContentMin &&
        seg.box.height < seg.box.y &&
        largerRtree.searchIntersection(up.toArchery).isEmpty)
        Seq(ImageSegment(imp, up))
      else Seq()) ++
        (if (math.abs(segContent-downContent) < ContentDiff &&
          downContent > ContentMin &&
          seg.box.y+seg.box.height*2 < imp3.getHeight &&
          largerRtree.searchIntersection(down.toArchery).isEmpty)
          Seq(ImageSegment(imp, down))
        else Seq()) ++
        (if (math.abs(segContent-leftContent) < ContentDiff &&
          leftContent > ContentMin &&
          seg.box.width < seg.box.x &&
          largerRtree.searchIntersection(left.toArchery).isEmpty)
          Seq(ImageSegment(imp, left))
        else Seq()) ++
        (if (math.abs(segContent-rightContent) < ContentDiff &&
          rightContent > ContentMin &&
          seg.box.x+seg.box.width*2 < imp3.getWidth &&
          largerRtree.searchIntersection(right.toArchery).isEmpty)
          Seq(ImageSegment(imp, right))
        else Seq())
    }
    segs = larger ++ recoverMissing
    log(imp2, "[GappedImageSegmenter] recover missing panels", recoverMissing.map{s=>s.box.toRoi}: _*)

    //check segmentation area
    val segmentArea = segs.map{s=>(s.box.x2-s.box.x)*(s.box.y2-s.box.y)}.sum
    val imageArea = imp2.getWidth * imp2.getHeight
    if (segmentArea.toDouble / imageArea.toDouble < SegmentAreaCutoff) {
      logger.warn(s"Image segment area is less than $SegmentAreaCutoff, skipping")
      return Seq()
    }
    // recover small components
    @tailrec def
    recoverSmallComponents( smaller: Seq[ImageSegment],
                            segments: Seq[ImageSegment]): Seq[ImageSegment] =
    {
      smaller match {
        case small :: ss =>
          val nearest = segments.
            sortBy {s => GappedImageSegmenter.rectDistance(small.box, s.box)}.
            headOption
          nearest match {
            case Some(n) =>
              recoverSmallComponents(ss,
                segments.filter{_ != n} ++
                  Seq(ImageSegment(
                    small.imp,
                    ImageSegmenter.Box(
                      math.min(small.box.x, n.box.x),
                      math.min(small.box.y, n.box.y),
                      math.max(small.box.x2, n.box.x2),
                      math.max(small.box.y2, n.box.y2)))))
            case None => segments
          }
        case _ => segments
      }
    }
    val recovered = recoverSmallComponents(smaller ++ smallSegs, segs)
    segs = ArrayBuffer(recovered: _*)
    log(imp2, "[GappedImageSegmenter] recover small components", recovered.map{s=>s.box.toRoi}: _*)
    segs
  }
}

object GappedImageSegmenter {
  def distance[T](x: T, y: T, x2: T, y2: T)(implicit num: Numeric[T]): Double = {
    import num._
    math.sqrt(((x2 - x)*(x2 - x) + (y2 - y) * (y2 - y)).toDouble)
  }
  // https://stackoverflow.com/questions/4978323/how-to-calculate-distance-between-two-rectangles-context-a-game-in-lua
  def rectDistance[T](box1: ImageSegmenter.Box[T], box2: ImageSegmenter.Box[T])(implicit num: Numeric[T]): Double = {
    import num._
    val left = box2.x2 < box1.x
    val right = box1.x2 < box2.x
    val bottom = box2.y2 < box1.y
    val top = box1.y2 < box2.y
    if (top && left) distance(box1.x, box1.y2, box2.x2, box2.y)
    else if (left && bottom) distance(box1.x, box1.y, box2.x2, box2.y2)
    else if (bottom && right) distance(box1.x2, box1.y, box2.x, box2.y2)
    else if (right && top) distance(box1.x2, box1.y2, box2.x, box2.y)
    else if (left) (box1.x - box2.x2).toDouble
    else if (right) (box2.x - box1.x2).toDouble
    else if (bottom) (box1.y - box2.y2).toDouble
    else if (top) (box2.y - box1.y2).toDouble
    // rectangles intersect
    else 0
  }
}
