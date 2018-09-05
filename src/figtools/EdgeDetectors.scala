package figtools

import ij.process.FloatProcessor
import ij.{IJ, ImagePlus}
import org.openimaj.image.FImage
import org.openimaj.image.processing.edges.SUSANEdgeDetector
import com.typesafe.scalalogging.Logger

object EdgeDetectors {
  val logger = Logger(getClass.getSimpleName)

  trait EdgeDetector {
    def run(imp: ImagePlus)(implicit log: ImageLog): ImagePlus
  }

  case object ImageJ extends EdgeDetector {
    override def run(imp: ImagePlus)(implicit log: ImageLog): ImagePlus = {
      logger.info("running ImageJ edge detector, this may take some time...")
      val edgeImage = imp.duplicate()
      IJ.run(edgeImage, "Find Edges", "")
      log(edgeImage, "[EdgeDetectors: ImageJ] edge image")
      // binarize the edge image
      IJ.run(edgeImage, "Make Binary", "")
      log(edgeImage, "[EdgeDetectors: ImageJ] binarized edge image")
      edgeImage
    }
  }
  case object Susan extends EdgeDetector {
    override def run(imp: ImagePlus)(implicit log: ImageLog): ImagePlus = {
      logger.info("running Susan edge detector, this may take some time...")
      val fimage = new FImage(imp.getProcessor.getFloatArray)
      val Threshold = 0.08
      val NMax = 9
      val Radius = 3.4
      val susan = SUSANEdgeDetector.smoothCircularSusan(fimage, Threshold, NMax, Radius)
      val edgeImage = new ImagePlus(
        imp.getTitle,
        new FloatProcessor(susan.pixels).convertToByteProcessor())
      log(edgeImage, "[EdgeDetectors: Susan] edge image")
      // binarize the edge image
      IJ.run(edgeImage, "Make Binary", "")
      log(edgeImage, "[EdgeDetectors: Susan] binarized edge image")
      edgeImage
    }
  }
}
