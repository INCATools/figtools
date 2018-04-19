package figtools

import com.conversantmedia.util.collection.geometry.Rect2d
import ij.ImagePlus

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]

  case class ImageSegment(imp: ImagePlus, box: Rect2d)
}
