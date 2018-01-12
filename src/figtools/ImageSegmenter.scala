package figtools

import java.awt.Rectangle

import ij.ImagePlus

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]

  case class ImageSegment(imp: ImagePlus, box: Rectangle)
}
