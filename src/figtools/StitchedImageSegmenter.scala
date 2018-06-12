package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.IJ
import ij.process.FloatProcessor
import org.openimaj.image.FImage
import org.openimaj.image.processing.edges.SUSANEdgeDetector
import ImageLog.log
import com.typesafe.scalalogging.Logger

class StitchedImageSegmenter extends ImageSegmenter {
  val logger = Logger("FigTools")

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    // get edge image
    logger.info("running SUSAN edge detector, this may take some time...")
    val fimage = new FImage(imp.getProcessor.getFloatArray)
    val Threshold = 0.08
    val NMax = 9
    val Radius = 3.4
    val susan = SUSANEdgeDetector.smoothCircularSusan(fimage, Threshold, NMax, Radius)
    val edgeImage = new ImagePlus(
      imp.getTitle,
      new FloatProcessor(susan.pixels).convertToByteProcessor())
    log(edgeImage, "[StitchedImageSegmenter] edge image")
    // binarize the edge image
    IJ.run(edgeImage, "Make Binary", "")
    log(edgeImage, "[StitchedImageSegmenter] binarized edge image")

    val segments = segment0(ImageSegment(edgeImage, ImageSegmenter.Box(0, 0, imp.getWidth, imp.getHeight)))
    log(imp, "[StitchedImageSegmenter] split into segments", segments.map{s=>s.box.toRoi}: _*)
    val MinLengthRatio = 0.2
    val result = segments.filter{seg=>seg.box.width >= MinLengthRatio && seg.box.height >= MinLengthRatio}
    log(imp, s"[StitchedImageSegmenter] filter segments by MinLengthRatio $MinLengthRatio",
      result.map{s=>s.box.toRoi}: _*)
    result
  }

  def segment0(segment: ImageSegment): Seq[ImageSegment] = {
    val imp = segment.imp

    // get highest peak greater than MinPeakRatio
    val MinPeakRatio = 0.7
    val horizProj = Array.ofDim[Long](segment.box.x2-segment.box.x+1)
    for (y <- segment.box.y to segment.box.y2) {
      for (x <- segment.box.x to segment.box.x2) {
        horizProj(x-segment.box.x) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
      }
    }
    val vertProj = Array.ofDim[Long](segment.box.y2-segment.box.y+1)
    for (x <- segment.box.x to segment.box.x2) {
      for (y <- segment.box.y to segment.box.y2) {
        vertProj(y-segment.box.y) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
      }
    }
    var bestHoriz = 0L
    var bestHorizX = -1
    var bestVert = 0L
    var bestVertY = -1
    for (x <- segment.box.x to segment.box.x2) {
      if (horizProj(x-segment.box.x) >= MinPeakRatio * (segment.box.y2-segment.box.y+1) && horizProj(x-segment.box.x) > bestHoriz) {
        bestHoriz = horizProj(x-segment.box.x)
        bestHorizX = x
      }
    }
    for (y <- segment.box.y to segment.box.y2) {
      if (vertProj(y-segment.box.y) >= MinPeakRatio * (segment.box.x2-segment.box.x+1) && vertProj(y-segment.box.y) > bestVert) {
        bestVert = vertProj(y-segment.box.y)
        bestVertY = y
      }
    }

    // if no peak exists, return whole segment
    val segments = if (bestHoriz == 0 && bestVert == 0) {
      Seq(segment)
    }
    // return horizontally split segments
    else if (bestHoriz > bestVert) {
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        segment.box.x,
        segment.box.y,
        bestHorizX,
        segment.box.y2))) ++
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        bestHorizX,
        segment.box.y,
        segment.box.x2,
        segment.box.y2)))
    }
    // return vertically split segments
    else {
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        segment.box.x,
        segment.box.y,
        segment.box.x2,
        bestVertY))) ++
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        segment.box.x,
        bestVertY,
        segment.box.x2,
        segment.box.y2)))
    }
    segments
  }
}
