package figtools
import archery.Box
import ij.ImagePlus
import ij.gui.Roi
import ij.process.{ImageConverter, ImageProcessor}
import ImageLog.log

import scala.collection.mutable
import scala.util.control.Breaks._

class ImagePreprocessor {
  def preprocess(imp: ImagePlus): ImagePlus = {
    log(imp, "[ImagePreprocessor] original image")

    // convert to 8-bit grayscale
    new ImageConverter(imp).convertToGray8()
    log(imp, "[ImagePreprocessor] convert to gray 8-bit")
    // resize to 2x
    val resized = new ImagePlus(imp.getTitle())
    imp.getProcessor().setInterpolationMethod(ImageProcessor.BICUBIC)
    resized.setProcessor(imp.getProcessor.resize(imp.getWidth() * 2, imp.getHeight() * 2))
    log(resized, "[ImagePreprocessor] resize to 2x")
    // linear map the pixel values to [0.05, 0.95]
    val minPixel = (0.05 * 255.0).toInt
    val maxPixel = (0.95 * 255.0).toInt
    for (y <- 0 until resized.getHeight) {
      for (x <- 0 until resized.getWidth) {
        val pixel = resized.getPixel(x, y)
        for (i <- 0 until pixel.length) {
          val value = if (pixel(i) < minPixel) minPixel
          else if (pixel(i) > maxPixel) maxPixel
          else ((pixel(i).toDouble / (maxPixel - minPixel).toDouble) * 256.0).toInt
          resized.getProcessor().putPixel(x, y, value)
        }
      }
    }
    log(resized, "[ImagePreprocessor] linear map the pixel values to 0.05, 0.95")
    // crop image borders by removing rows and columns of pixels whose maximum
    // gradient value is 0.
    val edges = new ImagePlus(resized.getTitle(), resized.getProcessor().duplicate())
    edges.getProcessor().findEdges()
    log(edges, "[ImagePreprocessor] find edges")
    val cropped = cropImageBorders(edges, 255.toByte.toInt).getOrElse(resized)
    log(cropped, "[ImagePreprocessor] crop image borders")
    // figure out if image is black or white background
    // compute histogram of pixel color values
    val histo = new mutable.HashMap[Int,Long]()
    for (y <- 0 until cropped.getHeight) {
      for (x <- 0 until cropped.getWidth) {
        val pixel = imp.getPixel(x, y)
        if (pixel.length >= 3) {
          val value = pixel(0)
          histo += value->(histo.getOrElse(value, 0L)+1L)
        }
        else if (pixel.length >= 1) {
          histo += pixel(0)->(histo.getOrElse(pixel(0), 0L)+1L)
        }
      }
    }
    // get the modal color value for the image
    var mode = 0
    var modeCount = 0L
    for ((k,v) <- histo) {
      if (modeCount < v) {
        mode = k
        modeCount = v
      }
    }
    // if this image has a black background, invert it
    if (mode < (0.5 * 256.0).toInt) {
      cropped.getProcessor().invert()
      log(cropped, "[ImagePreprocessor] invert image")
    }
    cropped
  }

  def findImageBorders(imp: ImagePlus, gapColor: Int): Box = {
    var x1 = 0
    var x2 = imp.getWidth() - 1
    var y1 = 0
    var y2 = imp.getHeight() - 1
    var cropX1 = -1
    var cropX2 = -1
    var cropY1 = -1
    var cropY2 = -1
    while ((cropX1 < 0 || cropY1 < 0 || cropX2 < 0 || cropY2 < 0) && x1 < x2 && y1 < y2) {
      if (cropX1 < 0) {
        var gapValues = 0
        breakable {
          for (y <- y1 until y2) {
            val value = imp.getPixel(x1, y)(0)
            if (value == gapColor) {
              x1 += 1
              gapValues += 1
              break
            }
          }
        }
        if (gapValues == 0) {
          cropX1 = x1
        }
      }
      if (cropX2 < 0) {
        var gapValues = 0
        breakable {
          for (y <- y1 until y2) {
            val value = imp.getPixel(x2, y)(0)
            if (value == gapColor) {
              x2 -= 1
              gapValues += 1
              break
            }
          }
        }
        if (gapValues == 0) {
          cropX2 = x2
        }
      }
      if (cropY1 < 0) {
        var gapValues = 0
        breakable {
          for (x <- x1 until x2) {
            val value = imp.getPixel(x, y1)(0)
            if (value == gapColor) {
              y1 += 1
              gapValues += 1
              break
            }
          }
        }
        if (gapValues == 0) {
          cropY1 = y1
        }
      }
      if (cropY2 < 0) {
        var gapValues = 0
        breakable {
          for (x <- x1 until x2) {
            val value = imp.getPixel(x, y2)(0)
            if (value == gapColor) {
              y2 -= 1
              gapValues += 1
              break
            }
          }
        }
        if (gapValues == 0) {
          cropY2 = y2
        }
      }
    }
    // cropping failed
    val MinSizeRatio = 0.4
    val MinWidth =  (imp.getWidth() * MinSizeRatio).toInt
    val MinHeight =  (imp.getHeight() * MinSizeRatio).toInt
    if ((cropX1 < 0 || cropY1 < 0 || cropX2 < 0 || cropY2 < 0) || !(cropX1 + MinWidth < cropX2 && cropY1 + MinHeight < cropY2)) {
      Box(x1, x2, y1, y2)
    }
    else Box(cropX1, cropY1, cropX2 - cropX1 + 1, cropY2 - cropY1 + 1)
  }

  def cropImageBorders(imp: ImagePlus, gapColor: Int): Option[ImagePlus] = {
    val roi = findImageBorders(imp, gapColor)
    if (roi == Box(0, 0, imp.getWidth-1, imp.getHeight-1)) {
      None
    }
    else {
      imp.setRoi(new Roi(roi.x, roi.y, roi.x2 - roi.x + 1, roi.y2 - roi.y + 1))
      val cropped = new ImagePlus(imp.getTitle(), imp.getProcessor().crop())
      Some(cropped)
    }
  }
}

