package figtools

import archery.Box
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]
}

object ImageSegmenter {
  case class Box[T](x: T, y: T, x2: T, y2: T)(implicit num: Numeric[T]) {
    import num._
    def width: T = x2 - x
    def height: T = y2 - y
    def toArchery: archery.Box = archery.Box(x.toFloat, y.toFloat, x2.toFloat, y2.toFloat)
  }
  case class ImageSegment(imp: ImagePlus, box: Box[Int])

  def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val preprocessed = new ImagePreprocessor().preprocess(imp)
    var segments = new GappedImageSegmenter().segment(preprocessed)
    if (segments.isEmpty) {
      segments = new StitchedImageSegmenter().segment(preprocessed)
    }
    if (segments.isEmpty) {
      segments = new BlurryFragmentedImageSegmenter().segment(preprocessed)
    }
    segments
  }
}