package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ImageLog.log
import com.typesafe.scalalogging.Logger

class StitchedImageSegmenter extends ImageSegmenter {
  val logger = Logger(getClass.getSimpleName)

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val edgeDetector = FigTools.edgeDetectors(FigTools.edgeDetector)
    val edgeImage = edgeDetector.run(imp)

    val segments = segment0(ImageSegment(edgeImage,
      ImageSegmenter.Box(0, 0, imp.getWidth-1, imp.getHeight-1)))
    log(imp, "[StitchedImageSegmenter] split into segments",
      segments.zipWithIndex.map{case (s,i)=>s"seg${i+1}"->s.box.toRoi}: _*)
    val MinLengthRatio = 0.2
    val result = segments.filter{seg=>seg.box.width >= MinLengthRatio && seg.box.height >= MinLengthRatio}
    log(imp, s"[StitchedImageSegmenter] filter segments by MinLengthRatio $MinLengthRatio",
      result.zipWithIndex.map{case (s,i)=>s"seg${i+1}"->s.box.toRoi}: _*)
    result
  }

  def segment0(segment: ImageSegment): Seq[ImageSegment] = {
    val imp = segment.imp

    // get highest peak greater than MinPeakRatio
    val MinPeakRatio = 0.7
    val horizProj = Array.ofDim[Long](segment.box.width)
    val vertProj = Array.ofDim[Long](segment.box.height)
    for {
      y <- segment.box.y to segment.box.y2
      x <- segment.box.x to segment.box.x2
    } {
      horizProj(x-segment.box.x) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
      vertProj(y-segment.box.y) += (if (imp.getProcessor.getPixel(x, y) == 0) 0 else 1)
    }
    val MinSegmentSize = 0.1
    var bestHoriz = 0L
    var bestHorizX = -1
    for (x <- segment.box.x to segment.box.x2) {
      if (horizProj(x-segment.box.x) >= MinPeakRatio * segment.box.height &&
        horizProj(x-segment.box.x) > bestHoriz &&
        x-segment.box.x+1 >= MinSegmentSize * segment.imp.getWidth &&
        segment.box.x2-x+1 >= MinSegmentSize * segment.imp.getWidth)
      {
        bestHoriz = horizProj(x-segment.box.x)
        bestHorizX = x
      }
    }
    var bestVert = 0L
    var bestVertY = -1
    for (y <- segment.box.y to segment.box.y2) {
      if (vertProj(y-segment.box.y) >= MinPeakRatio * segment.box.width &&
        vertProj(y-segment.box.y) > bestVert &&
        y-segment.box.y+1 >= MinSegmentSize * segment.imp.getHeight &&
        segment.box.y2-y+1 >= MinSegmentSize * segment.imp.getHeight)
      {
        bestVert = vertProj(y-segment.box.y)
        bestVertY = y
      }
    }

    // if no peak exists, return whole segment
    if (bestHoriz == 0 && bestVert == 0) {
      Seq(segment)
    }
    // return horizontally split segments
    else if (bestHoriz > bestVert) {
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        segment.box.x,
        segment.box.y,
        math.max(0,bestHorizX-1),
        segment.box.y2))) ++
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        math.min(segment.box.x2,bestHorizX+1),
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
        math.max(0,bestVertY-1)))) ++
      segment0(ImageSegment(imp, ImageSegmenter.Box(
        segment.box.x,
        math.min(segment.box.y2,bestVertY+1),
        segment.box.x2,
        segment.box.y2)))
    }
  }
}
