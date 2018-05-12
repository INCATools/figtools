package figtools

import archery.Box
import ij.ImagePlus

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]

  case class ImageSegment(imp: ImagePlus, box: Box)
}
