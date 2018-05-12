package figtools

import archery.{Box, Entry, RTree}
import com.typesafe.scalalogging.Logger
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
    var rtree = RTree((0 until rt.getCounter).flatMap{i=>
      val bx = rt.getValue("BX", i).toFloat
      val by = rt.getValue("BY", i).toFloat
      val width = rt.getValue("Width", i).toFloat
      val height = rt.getValue("Height", i).toFloat

      if (width > imp2.getWidth / ParticleThreshold &&
          height > imp2.getHeight / ParticleThreshold)
      {
        val box = Box(bx, by, bx+width, by+height)
        segs += ImageSegment(imp, box)
        Some(Entry(box, box))
      }
      else None
    }: _*)
    // merge overlapping segments
    var loop = true
    while (loop) {
      loop = false
      val outsegs = ArrayBuffer[ImageSegment]()
      for (seg <- segs) {
        for (t <- rtree.search(seg.box)) {
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
              val combinedBox = Box(
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
    segs = segs.sortBy({seg=>
        (seg.box.x2 - seg.box.x) *
        (seg.box.y2 - seg.box.y)
      }).reverse.headOption match
    {
      case Some(largest) =>
        segs.filter(seg=>
          seg.box.x2 - seg.box.x /
            largest.box.x2 - largest.box.x
            > BboxThreshold &&
          seg.box.y2 - seg.box.y /
            largest.box.y2 - largest.box.y
            > BboxThreshold)
      case None =>
        logger.warn(s"No largest bounding box found!")
        segs
    }
    // recover missing panels
    for {
      maxWidthEntry <- segs.sortBy(_.box.width).reverse.headOption
      maxWidth = maxWidthEntry.box.width
      maxHeightEntry <- segs.sortBy(_.box.height).reverse.headOption
      maxHeight = maxHeightEntry.box.height
    } {
      segs = segs.flatMap{seg=>
        val up = Box(seg.box.x,seg.box.y-seg.box.height,seg.box.x2,seg.box.y-1)
        val down = Box(seg.box.x,seg.box.y+seg.box.height+1,seg.box.x2,seg.box.y2+seg.box.height)
        val left = Box(seg.box.x-seg.box.width,seg.box.y,seg.box.x-1,seg.box.y2)
        val right = Box(seg.box.x+seg.box.width+1,seg.box.y,seg.box.x2+seg.box.width,seg.box.y2)

        (if (seg.box.height < seg.box.y && rtree.search(up).isEmpty)
          Seq(ImageSegment(imp, up))
        else Seq.empty) ++
          (if (seg.box.y+seg.box.height*2 < maxHeight && rtree.search(down).isEmpty)
            Seq(ImageSegment(imp, down))
          else Seq.empty) ++
          (if (seg.box.width < seg.box.x && rtree.search(left).isEmpty)
            Seq(ImageSegment(imp, left))
          else Seq.empty) ++
          (if (seg.box.x+seg.box.width*2 < maxWidth && rtree.search(right).isEmpty)
            Seq(ImageSegment(imp, right))
          else Seq.empty)
      }
    }
    segments
  }
}
