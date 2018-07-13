package figtools

import archery.{Box, Entry, RTree}
import com.typesafe.scalalogging.Logger
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer

import scala.collection.mutable.ArrayBuffer
import ImageLog.log
import ij.gui.Roi

class GappedImageSegmenter extends ImageSegmenter {
  val logger = Logger(getClass.getSimpleName)
  val BinarizeThreshold = 0.95
  val ParticleThreshold = 20.0

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val imp2 = imp.duplicate()
    //log(imp2, "[GappedImageSegmenter] original image")
    val segments = ArrayBuffer[ImageSegment]()

    // binarize the image using a threshold of 0.95
    val threshold = (255.0 * BinarizeThreshold).toInt
    val lut = (0 until 256).map{v=>if (v < threshold) 0 else 255.toByte.toInt}.toArray
    imp2.getProcessor.applyTable(lut)
    log(imp2, s"[GappedImageSegmenter] Binarize with $threshold threshold")

    val rt = new ResultsTable
    val particleAnalyzer = new ParticleAnalyzer(
      ParticleAnalyzer.EXCLUDE_EDGE_PARTICLES |
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
    val boxes = ArrayBuffer[Roi]()
    val rtree = RTree((0 until rt.getCounter).flatMap{i=>
      val bx = rt.getValue("BX", i).toInt
      val by = rt.getValue("BY", i).toInt
      val width = rt.getValue("Width", i).toInt
      val height = rt.getValue("Height", i).toInt

      if (width > imp2.getWidth / ParticleThreshold &&
          height > imp2.getHeight / ParticleThreshold)
      {
        val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
        boxes += box.toRoi
        segs += ImageSegment(imp, box)
        Some(Entry(Box(box.x, box.y, box.x2, box.y2), box))
      }
      else None
    }: _*)
    log(imp2, "[GappedImageSegmenter] RTree boxes", boxes: _*)
    // merge overlapping segments
    var loop = true
    while (loop) {
      loop = false
      val outsegs = ArrayBuffer[ImageSegment]()
      for (seg <- segs) {
        for (t <- rtree.search(seg.box.toArchery)) {
          if (t.value != seg.box) {
            val intersectionBox = Box(
              math.max(seg.box.x, t.value.x),
              math.max(seg.box.y, t.value.y),
              math.min(seg.box.x, t.value.x),
              math.min(seg.box.y, t.value.y))
            val intersectionArea =
              (intersectionBox.x2 - intersectionBox.x) *
              (intersectionBox.y2 - intersectionBox.y)
            val minBoxArea =
              (seg.box.x2 - seg.box.x) *
              (seg.box.y2 - seg.box.y)
            val MergeThreshold = 0.1
            if (intersectionArea / minBoxArea > MergeThreshold) {
              loop = true
              val combinedBox = ImageSegmenter.Box(
                math.min(seg.box.x, t.value.x),
                math.min(seg.box.y, t.value.y),
                math.max(seg.box.x, t.value.x),
                math.max(seg.box.y, t.value.y))
              outsegs += ImageSegment(seg.imp, combinedBox)
            }
          }
        }
      }
      segs = outsegs
    }
    log(imp2, "[GappedImageSegmenter] merge overlapping segments", segs.map{s=>s.box.toRoi}: _*)
    // temporarily eliminate small components
    val BboxThreshold = 0.2
    val (larger, smaller) = segs.sortBy({seg=>
        (seg.box.x2 - seg.box.x) *
        (seg.box.y2 - seg.box.y)
      }).reverse.headOption match
    {
      case Some(largest) =>
        val larger = segs.filter(seg=>
          seg.box.x2 - seg.box.x /
            largest.box.x2 - largest.box.x
            > BboxThreshold &&
          seg.box.y2 - seg.box.y /
            largest.box.y2 - largest.box.y
            > BboxThreshold)
        val smaller = segs.filter(seg=>
          seg.box.x2 - seg.box.x /
            largest.box.x2 - largest.box.x
            <= BboxThreshold &&
            seg.box.y2 - seg.box.y /
              largest.box.y2 - largest.box.y
              <= BboxThreshold)
        (larger, smaller)
      case None =>
        logger.warn(s"No largest bounding box found!")
        (segs, Seq())
    }
    log(imp2, "[GappedImageSegmenter] temporarily eliminate small components", larger.map{s=>s.box.toRoi}: _*)
    // recover missing panels
    for {
      maxWidthEntry <- segs.sortBy(_.box.width).reverse.headOption
      maxWidth = maxWidthEntry.box.width
      maxHeightEntry <- segs.sortBy(_.box.height).reverse.headOption
      maxHeight = maxHeightEntry.box.height
    } {
      segs = larger.flatMap{seg=>
        val up = ImageSegmenter.Box(seg.box.x,seg.box.y-seg.box.height,seg.box.x2,seg.box.y-1)
        val down = ImageSegmenter.Box(seg.box.x,seg.box.y+seg.box.height+1,seg.box.x2,seg.box.y2+seg.box.height)
        val left = ImageSegmenter.Box(seg.box.x-seg.box.width,seg.box.y,seg.box.x-1,seg.box.y2)
        val right = ImageSegmenter.Box(seg.box.x+seg.box.width+1,seg.box.y,seg.box.x2+seg.box.width,seg.box.y2)

        (if (seg.box.height < seg.box.y && rtree.search(up.toArchery).isEmpty)
          Seq(ImageSegment(imp, up))
        else Seq.empty) ++
          (if (seg.box.y+seg.box.height*2 < maxHeight && rtree.search(down.toArchery).isEmpty)
            Seq(ImageSegment(imp, down))
          else Seq.empty) ++
          (if (seg.box.width < seg.box.x && rtree.search(left.toArchery).isEmpty)
            Seq(ImageSegment(imp, left))
          else Seq.empty) ++
          (if (seg.box.x+seg.box.width*2 < maxWidth && rtree.search(right.toArchery).isEmpty)
            Seq(ImageSegment(imp, right))
          else Seq.empty)
      }
    }
    log(imp2, "[GappedImageSegmenter] recover missing panels", segs.map{s=>s.box.toRoi}: _*)
    //check segmentation area
    val segmentArea = segments.map{s=>(s.box.x2-s.box.x)*(s.box.y2-s.box.y)}.sum
    val imageArea = imp2.getWidth * imp2.getHeight
    val SegmentAreaCutoff = 0.5
    if (segmentArea / imageArea < SegmentAreaCutoff) {
      logger.warn(s"Image segment area is less than $SegmentAreaCutoff, skipping")
      return Seq()
    }
    // recover small components
    val NewBoxPctCutoff = 0.2
    val recovered = ArrayBuffer[ImageSegment]()
    for (small <- smaller) {
      var nearest: Option[ImageSegment] = None
      var nearestDistance: Option[Double] = None
      for (seg <- segs) {
        val distance = GappedImageSegmenter.rectDistance(small.box, seg.box)
        if (nearest.isEmpty || distance < nearestDistance.get) {
          nearest = Some(seg)
          nearestDistance = Some(distance)
        }
      }
      for { near <- nearest } {
        val newBox = ImageSegmenter.Box(math.min(small.box.x, near.box.x),
            math.min(small.box.y, near.box.y),
            math.max(small.box.x2, near.box.x2),
            math.max(small.box.y2, near.box.y2))
        if (newBox.x2-newBox.x != near.box.x2-near.box.x &&
            newBox.y2-newBox.y != near.box.y2-near.box.y)
        {
          recovered += near
        }
        else {
          if (newBox.x2-newBox.x > NewBoxPctCutoff*(near.box.x2-near.box.x) ||
              newBox.y2-newBox.y > NewBoxPctCutoff*(near.box.y2-near.box.y))
          {
            recovered += small
            recovered += near
          }
          else {
            recovered += ImageSegment(small.imp, newBox)
          }
        }
      }
    }
    log(imp2, "[GappedImageSegmenter] recover small components", recovered.map{s=>s.box.toRoi}: _*)
    recovered
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
