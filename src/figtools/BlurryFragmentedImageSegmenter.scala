package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.process.FloatProcessor
import ij.{IJ, ImagePlus}
import org.openimaj.image.FImage
import org.openimaj.image.processing.edges.SUSANEdgeDetector

import scala.collection.mutable.ArrayBuffer

class BlurryFragmentedImageSegmenter extends ImageSegmenter {
  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val segments = ArrayBuffer[ImageSegment]()

    val fimage = new FImage(imp.getProcessor.getFloatArray)
    val Threshold = 0.08
    val NMax = 9
    val Radius = 3.4
    val susan = SUSANEdgeDetector.smoothCircularSusan(fimage, Threshold, NMax, Radius)
    val edgeImage = new ImagePlus(
      imp.getTitle(),
      new FloatProcessor(susan.pixels).convertToByteProcessor())
    IJ.run(edgeImage, "Make Binary", "")

    // find minimum gap width using xycut

    // dilate using minimum gap width

    // apply cca method

    segments
  }
}
