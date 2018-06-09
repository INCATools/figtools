package figtools

import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.{IJ, ImagePlus, WindowManager}

object ImageLog {
  def log(imp: ImagePlus, description: String, rois: (String,Roi)*): ImagePlus = {
    // get ROI manager and update settings
    val rm = RoiManager.getRoiManager
    rm.runCommand("Associate", "true")
    rm.runCommand("Centered", "false")
    rm.runCommand("UseNames", "true")

    // get currently displayed image
    val currentImp = WindowManager.getCurrentImage
    if (currentImp == null) {
      FigTools.logger.warn("Could not log images: No active image")
      return imp
    }

    // resize canvas if necessary
    if (imp.getWidth > currentImp.getWidth || imp.getHeight > currentImp.getHeight) {
      val width = math.max(imp.getWidth, currentImp.getWidth)
      val height = math.max(imp.getHeight, currentImp.getHeight)
      IJ.run(currentImp, "Canvas Size...", s"width=$width height=$height position=Top-Left")
    }
    // make a new slice
    currentImp.setSlice(currentImp.getNSlices)
    currentImp.getImageStack.addSlice(description, imp.getProcessor.duplicate())
    currentImp.setSlice(currentImp.getNSlices)
    // add ROIs to new slice
    for ((label, roi) <- rois) {
      rm.addRoi(roi)
      val index = rm.getRoiIndex(roi)
      rm.rename(index, label)
    }
    imp
  }

  implicit class ImagePlusAdditions(imp: ImagePlus) {
    def log(description: String, rois: (String,Roi)*): ImagePlus = {
      ImageLog.log(imp, description, rois: _*)
    }
  }
}
