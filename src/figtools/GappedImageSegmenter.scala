package figtools
import java.awt.Rectangle
import java.util.function.Consumer

import com.conversantmedia.util.collection.geometry.Rect2d
import com.conversantmedia.util.collection.spatial.SpatialSearches
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
    val pixels = imp2.getProcessor.getPixels.asInstanceOf[Array[Byte]]
    val threshold = (255.0 * BinarizeThreshold).toInt
    for (i <- pixels.indices) {
      pixels(i) = if (pixels(i) < threshold) 0.toByte else 255.toByte
    }

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
    val rtree = SpatialSearches.rTree(Rect2d.Builder)
    for (i <- 0 until rt.getCounter) {
      val bx = rt.getValue("BX", i)
      val by = rt.getValue("BY", i)
      val width = rt.getValue("Width", i)
      val height = rt.getValue("Height", i)

      if (width > imp2.getWidth / ParticleThreshold &&
          height > imp2.getHeight / ParticleThreshold)
      {
        segs += ImageSegment(imp, new Rect2d(
          bx, by, bx+width, by+height))
        rtree.add(new Rect2d(bx, by, bx+width, by+height))
      }
    }
    // merge overlapping segments
    var loop = true
    while (loop) {
      loop = false
      val outsegs = ArrayBuffer[ImageSegment]()
      for (seg <- segs) {
        rtree.intersects(seg.box, new Consumer[Rect2d] {
          override def accept(t: Rect2d): Unit = {
            if (t != seg.box) {
              val intersectionBox = new Rect2d(
                math.max(seg.box.getMin.getCoord[Double](0), t.getMin.getCoord[Double](0)),
                math.max(seg.box.getMin.getCoord[Double](1), t.getMin.getCoord[Double](1)),
                math.min(seg.box.getMin.getCoord[Double](0), t.getMin.getCoord[Double](0)),
                math.min(seg.box.getMin.getCoord[Double](1), t.getMin.getCoord[Double](1)))
              val intersectionArea =
                (intersectionBox.getMax.getCoord[Double](0) - intersectionBox.getMin.getCoord[Double](0)) *
                (intersectionBox.getMax.getCoord[Double](1) - intersectionBox.getMin.getCoord[Double](1))
              val minBoxArea =
                (seg.box.getMax.getCoord[Double](0) - seg.box.getMin.getCoord[Double](0)) *
                  (seg.box.getMax.getCoord[Double](1) - seg.box.getMin.getCoord[Double](1))
              val MergeThreshold = 0.1
              if (intersectionArea / minBoxArea > MergeThreshold) {
                loop = true
                val combinedBox = new Rect2d(
                  math.min(seg.box.getMin.getCoord[Double](0), t.getMin.getCoord[Double](0)),
                  math.min(seg.box.getMin.getCoord[Double](1), t.getMin.getCoord[Double](1)),
                  math.max(seg.box.getMin.getCoord[Double](0), t.getMin.getCoord[Double](0)),
                  math.max(seg.box.getMin.getCoord[Double](1), t.getMin.getCoord[Double](1)))
                outsegs += ImageSegment(seg.imp, combinedBox)
              }
            }
          }
        })
      }
      segs = outsegs
    }
    // temporarily eliminate small components
    val BboxThreshold = 0.2
    segs = segs.sortBy({seg=>
        (seg.box.getMax.getCoord[Double](0) - seg.box.getMin.getCoord[Double](0)) *
          (seg.box.getMax.getCoord[Double](1) - seg.box.getMin.getCoord[Double](1))
      }).reverse.headOption match
    {
      case Some(largest) =>
        segs.filter(seg=>
          seg.box.getMax.getCoord[Double](0)-seg.box.getMin.getCoord[Double](0) /
            largest.box.getMax.getCoord[Double](0)-largest.box.getMin.getCoord[Double](0)
            > BboxThreshold &&
          seg.box.getMax.getCoord[Double](1)-seg.box.getMin.getCoord[Double](1) /
            largest.box.getMax.getCoord[Double](1)-largest.box.getMin.getCoord[Double](1)
            > BboxThreshold)
      case None =>
        logger.warn(s"No largest bounding box found!")
        segs
    }
    // recover missing panels

    segments
  }
}
