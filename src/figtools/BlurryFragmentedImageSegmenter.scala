package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.process.FloatProcessor
import ij.{IJ, ImagePlus}
import org.openimaj.image.FImage
import org.openimaj.image.processing.edges.SUSANEdgeDetector
import ImageLog.log
import com.typesafe.scalalogging.Logger

import scala.util.control.Breaks._

class BlurryFragmentedImageSegmenter extends ImageSegmenter {
  val logger = Logger("FigTools")

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    log(imp, "[BlurryFragmentedImageSegmenter] original image")

    // use SUSAN to create edge image
    logger.info("running SUSAN edge detector, this may take some time...")
    val fimage = new FImage(imp.getProcessor.getFloatArray)
    val Threshold = 0.08
    val NMax = 9
    val Radius = 3.4
    val susan = SUSANEdgeDetector.smoothCircularSusan(fimage, Threshold, NMax, Radius)
    val edgeImage = new ImagePlus(
      imp.getTitle,
      new FloatProcessor(susan.pixels).convertToByteProcessor())
    log(edgeImage, "[BlurryFragmentedImageSegmenter] edge image")
    // binarize the edge image
    IJ.run(edgeImage, "Make Binary", "")
    log(edgeImage, "[BlurryFragmentedImageSegmenter] binarized edge image")

    // find minimum gap width using xycut
    var minGapSize: Option[Int] = None
    for (y <- 0 until imp.getHeight) {
      var lastRow = -1
      breakable {
        for (x <- 0 until imp.getWidth) {
          if (imp.getProcessor.getPixel(x, y) != 0) {
            if (lastRow >= 0) {
              val gapSize = x-lastRow
              if (minGapSize.isEmpty || minGapSize.get > gapSize) minGapSize = Some(gapSize)
              lastRow = -1
            }
            break
          }
        }
        if (lastRow == -1) lastRow = y
      }
    }
    for (x <- 0 until imp.getWidth) {
      var lastCol = -1
      breakable {
        for (y <- 0 until imp.getHeight) {
          if (imp.getProcessor.getPixel(x, y) != 0) {
            if (lastCol >= 0) {
              val gapSize = x-lastCol
              if (minGapSize.isEmpty || minGapSize.get > gapSize) minGapSize = Some(gapSize)
              lastCol = -1
            }
            break
          }
        }
        if (lastCol == -1) lastCol = x
      }
    }

    // dilate using minimum gap width
    for (mgs <- minGapSize) {
      IJ.run(edgeImage, "Gray Morphology", s"radius=$mgs type=circle operator=dilate")
      log(edgeImage, "[BlurryFragmentedImageSegmenter] dilated edge image")
    }

    // apply cca method
    new GappedImageSegmenter().segment(edgeImage)
  }
}
