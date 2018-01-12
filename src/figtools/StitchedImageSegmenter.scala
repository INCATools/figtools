package figtools

import ij.ImagePlus

import scala.collection.mutable.ArrayBuffer

class StitchedImageSegmenter extends ImageSegmenter {
  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val segments = ArrayBuffer[ImageSegment]()

    segments
  }
}
