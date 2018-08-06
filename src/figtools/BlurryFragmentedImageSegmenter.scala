package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.{IJ, ImagePlus}
import ImageLog.log
import com.typesafe.scalalogging.Logger

import scala.util.control.Breaks._
import de.sciss.equal.Implicits._

class BlurryFragmentedImageSegmenter extends ImageSegmenter {
  val logger = Logger(getClass.getSimpleName)

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    //log(imp, "[BlurryFragmentedImageSegmenter] original image")

    val edgeDetector = FigTools.edgeDetectors(FigTools.edgeDetector)
    val edgeImage = edgeDetector.run(imp)

    // find minimum gap width using xycut
    var minGapSize: Option[Int] = None
    for (y <- 0 until edgeImage.getHeight) {
      var lastRow = -1
      breakable {
        for (x <- 0 until edgeImage.getWidth) {
          if (edgeImage.getProcessor.getPixel(x, y) !== 0) {
            if (lastRow >= 0) {
              val gapSize = x-lastRow
              if (minGapSize.isEmpty || minGapSize.get > gapSize) minGapSize = Some(gapSize)
              lastRow = -1
            }
            break
          }
        }
        if (lastRow === -1) lastRow = y
      }
    }
    for (x <- 0 until edgeImage.getWidth) {
      var lastCol = -1
      breakable {
        for (y <- 0 until edgeImage.getHeight) {
          if (edgeImage.getProcessor.getPixel(x, y) !== 0) {
            if (lastCol >= 0) {
              val gapSize = x-lastCol
              if (minGapSize.isEmpty || minGapSize.get > gapSize) minGapSize = Some(gapSize)
              lastCol = -1
            }
            break
          }
        }
        if (lastCol === -1) lastCol = x
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
