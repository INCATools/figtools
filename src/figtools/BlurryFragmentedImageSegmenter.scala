package figtools

import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus

import scala.collection.mutable.ArrayBuffer

class BlurryFragmentedImageSegmenter extends ImageSegmenter {
  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val segments = ArrayBuffer[ImageSegment]()

    segments
  }
}
