package figtools
import ij.ImagePlus

import scala.collection.mutable.ArrayBuffer

class GappedImageSegmenter extends ImageSegmenter {
  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val segments = ArrayBuffer[ImageSegment]()
    // resize to 2x
    val resized = new ImagePlus(imp.getTitle())
    resized.setProcessor(imp.getProcessor.resize(imp.getWidth()*2, imp.getHeight()*2))

    segments
  }
}
