package figtools

import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.{IJ, ImagePlus, WindowManager}

object ImageLog {
  def log(imp: ImagePlus, description: String, rois: Any*): ImagePlus = {
    // get ROI manager and update settings
    val rm = RoiManager.getRoiManager
    rm.runCommand("Associate", "true")
    rm.runCommand("Centered", "false")
    rm.runCommand("UseNames", "true")

    // get currently displayed image
    val currentImp = WindowManager.getCurrentImage
    if (currentImp == null) {
      throw new RuntimeException("Could not log images: No active image")
    }
    currentImp.setSlice(currentImp.getNSlices)
    // resize canvas if necessary
    if (imp.getWidth > currentImp.getWidth || imp.getHeight > currentImp.getHeight) {
      val width = math.max(imp.getWidth, currentImp.getWidth)
      val height = math.max(imp.getHeight, currentImp.getHeight)
      IJ.run(currentImp, "Canvas Size...", s"width=$width height=$height position=Top-Left")
    }
    // make a new slice
    currentImp.getImageStack.addSlice(description, imp.getProcessor.duplicate())
    currentImp.setSlice(currentImp.getNSlices)
    // add ROIs to new slice
    for (r <- rois) {
      r match {
        case (label: String,roi: Roi) =>
          rm.addRoi(roi)
          val index = rm.getRoiIndex(roi)
          rm.rename(index, label)
        case roi: Roi =>
          rm.addRoi(roi)
        case _ => throw new RuntimeException(s"Could not parse ROI argument: $r")
      }
    }
    currentImp
  }
}
