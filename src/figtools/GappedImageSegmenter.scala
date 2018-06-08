package figtools

import archery.{Box, Entry, RTree}
import com.typesafe.scalalogging.Logger
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer

import scala.collection.mutable.ArrayBuffer

class GappedImageSegmenter extends ImageSegmenter {
  val logger = Logger("FigTools")
  val BinarizeThreshold = 0.95
  val ParticleThreshold = 20.0

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val imp2 = imp.duplicate()
    val segments = ArrayBuffer[ImageSegment]()

    // binarize the image using a threshold of 0.95
    val threshold = (255.0 * BinarizeThreshold).toInt
    val lut = (0 until 256).map{v=>if (v < threshold) 0 else 255.toByte.toInt}.toArray
    imp.getProcessor.applyTable(lut)

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

    // Get the objects and iterate through them
    logger.info(s"ResultsTable Column Headings: ${rt.getColumnHeadings}")
    var segs = ArrayBuffer[ImageSegment]()
    val rtree = RTree((0 until rt.getCounter).flatMap{i=>
      val bx = rt.getValue("BX", i).toInt
      val by = rt.getValue("BY", i).toInt
      val width = rt.getValue("Width", i).toInt
      val height = rt.getValue("Height", i).toInt

      if (width > imp2.getWidth / ParticleThreshold &&
          height > imp2.getHeight / ParticleThreshold)
      {
        val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
        segs += ImageSegment(imp, box)
        Some(Entry(Box(box.x, box.y, box.x2, box.y2), box))
      }
      else None
    }: _*)
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
        val distance = rectDistance(small, seg)
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
    recovered
  }

  def distance(x: Float, y: Float, x2: Float, y2: Float): Double = {
    math.sqrt(math.pow(x2 - x, 2) + math.pow(y2 - y, 2))
  }

  // https://stackoverflow.com/questions/4978323/how-to-calculate-distance-between-two-rectangles-context-a-game-in-lua
  def rectDistance(seg1: ImageSegment, seg2: ImageSegment): Double = {
    val left = seg2.box.x2 < seg1.box.x
    val right = seg1.box.x2 < seg2.box.x
    val bottom = seg2.box.y2 < seg1.box.y
    val top = seg1.box.y2 < seg2.box.y
    if (top && left) distance(seg1.box.x, seg1.box.y2, seg2.box.x2, seg2.box.y)
    else if (left && bottom) distance(seg1.box.x, seg1.box.y, seg2.box.x2, seg2.box.y2)
    else if (bottom && right) distance(seg1.box.x2, seg1.box.y, seg2.box.x, seg2.box.y2)
    else if (right && top) distance(seg1.box.x2, seg1.box.y2, seg2.box.x, seg2.box.y)
    else if (left) seg1.box.x - seg2.box.x2
    else if (right) seg2.box.x - seg1.box.x2
    else if (bottom) seg1.box.y - seg2.box.y2
    else if (top) seg2.box.y - seg1.box.y2
    // rectangles intersect
    else 0
  }
}
