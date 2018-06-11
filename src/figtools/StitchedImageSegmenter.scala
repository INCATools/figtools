package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.IJ
import ij.process.FloatProcessor
import org.openimaj.image.FImage
import org.openimaj.image.processing.edges.SUSANEdgeDetector
import ImageLog.log

class StitchedImageSegmenter extends ImageSegmenter {
  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val segments = segment0(ImageSegment(imp, ImageSegmenter.Box(0, 0, imp.getWidth, imp.getHeight)))
    val MinLengthRatio = 0.2
    segments.filter{seg=>seg.box.width >= MinLengthRatio && seg.box.height >= MinLengthRatio}
  }

  def segment0(segment: ImageSegment): Seq[ImageSegment] = {
    val imp = segment.imp.duplicate()
    log(imp, "[StitchedImageSegmenter] original image")

    // get edge image
    val fimage = new FImage(imp.getProcessor.getFloatArray)
    val Threshold = 0.08
    val NMax = 9
    val Radius = 3.4
    val susan = SUSANEdgeDetector.smoothCircularSusan(fimage, Threshold, NMax, Radius)
    val edgeImage = new ImagePlus(
      imp.getTitle(),
      new FloatProcessor(susan.pixels).convertToByteProcessor())
    log(edgeImage, "[StitchedImageSegmenter] edge image")
    // binarize the edge image
    IJ.run(edgeImage, "Make Binary", "")
    log(edgeImage, "[StitchedImageSegmenter] binarized edge image")

    // get highest peak greater than MinPeakRatio
    val MinPeakRatio = 0.7
    val horizProj = Array.ofDim[Long](segment.box.width)
    for (y <- segment.box.y to segment.box.y2) {
      for (x <- segment.box.x to segment.box.x2) {
        horizProj(x) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
      }
    }
    val vertProj = Array.ofDim[Long](segment.box.height)
    for (x <- segment.box.x to segment.box.x2) {
      for (y <- segment.box.y to segment.box.y2) {
        vertProj(y) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
      }
    }
    var bestHoriz = 0L
    var bestHorizX = -1
    var bestVert = 0L
    var bestVertY = -1
    for (x <- segment.box.x to segment.box.x2) {
      if (horizProj(x) >= MinPeakRatio * segment.box.width && horizProj(x) > bestHoriz) {
        bestHoriz = horizProj(x)
        bestHorizX = x
      }
    }
    for (y <- segment.box.y to segment.box.y2) {
      if (vertProj(y) >= MinPeakRatio * segment.box.height && vertProj(y) > bestVert) {
        bestVert = vertProj(y)
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
    log(imp, "[StitchedImageSegmenter] split into segments", segments.map{s=>s.box.toRoi}: _*)
    segments
  }
}
