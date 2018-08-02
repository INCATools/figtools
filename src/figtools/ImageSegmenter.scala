package figtools

import com.github.davidmoten.rtree.geometry.{Geometries, Rectangle}
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.gui.Roi

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]
}

object ImageSegmenter {
  case class Box[T](x: T, y: T, x2: T, y2: T)(implicit num: Numeric[T]) {
    import num._
    def width: T = x2 - x + 1.asInstanceOf[T]
    def height: T = y2 - y + 1.asInstanceOf[T]
    def toRect: Rectangle = Geometries.rectangle(x.toFloat, y.toFloat, x2.toFloat, y2.toFloat)
    def toRoi: Roi = new Roi(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
  }
  case class ImageSegment(imp: ImagePlus, box: Box[Int])

  def segment(imp_ : ImagePlus): Seq[ImageSegment] = {
    val imp = imp_.duplicate()
    val preprocessed = new ImagePreprocessor().preprocess(imp)
    var segments = new GappedImageSegmenter().segment(preprocessed)
    if (segments.isEmpty) {
      segments = new StitchedImageSegmenter().segment(preprocessed)
    }
    if (segments.isEmpty) {
      segments = new BlurryFragmentedImageSegmenter().segment(preprocessed)
    }
    // resize the the segments by 0.5x back to 1x
    segments.map{s=>ImageSegment(s.imp,
      Box(s.box.x / 2, s.box.y / 2, s.box.x2 / 2, s.box.y2 / 2))}
  }
}