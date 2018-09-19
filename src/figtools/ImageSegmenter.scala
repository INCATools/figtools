package figtools

import com.github.davidmoten.rtree.geometry.{Geometries, Rectangle}
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.gui.Roi
import de.sciss.equal.Implicits._

trait ImageSegmenter {
  def segment(imp: ImagePlus): Seq[ImageSegment]
}

object ImageSegmenter {
  case class ImageSegment(imp: ImagePlus, box: Box) {
    override def equals( arg:Any): Boolean = arg match {
      case s: ImageSegment => s.box === box
      case _ => false
    }
    override def hashCode(): Int = box.hashCode
  }

  def segment(imp_ : ImagePlus)(implicit log: ImageLog): Seq[ImageSegment] = {
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