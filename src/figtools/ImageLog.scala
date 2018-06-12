package figtools

import java.util.Scanner

import com.typesafe.scalalogging.Logger
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.Blitter
import ij.{IJ, ImagePlus, WindowManager}
import javax.swing.SwingUtilities

object ImageLog {
  val logger = Logger("FigTools")
  def log(imp_ : ImagePlus, description: String, rois: Any*): Unit = {
    SwingUtilities.invokeAndWait(()=>{
      val imp = imp_.duplicate()
      // get currently displayed image
      val currentImp = WindowManager.getCurrentImage
      if (currentImp == null) {
        throw new RuntimeException("Could not log images: No active image")
      }
      // resize canvas if necessary
      if (imp.getWidth > currentImp.getWidth || imp.getHeight > currentImp.getHeight) {
        val width = math.max(imp.getWidth, currentImp.getWidth)
        val height = math.max(imp.getHeight, currentImp.getHeight)
        IJ.run(currentImp, "Canvas Size...", s"width=$width height=$height position=Top-Left")
      }
      // make a new slice
      currentImp.setSlice(currentImp.getNSlices)
      IJ.run(currentImp, "Add Slice", "")
      val sliceNum = currentImp.getNSlices
      currentImp.setSlice(sliceNum)
      // copy bits
      currentImp.getProcessor.copyBits(imp.getProcessor, 0, 0, Blitter.COPY)
      // set the slice label
      currentImp.getImageStack.setSliceLabel(description, sliceNum)

      // add ROIs to new slice
      if (rois.nonEmpty) {
        // get ROI manager and update settings
        val rm = RoiManager.getRoiManager
        rm.runCommand("Associate", "true")
        rm.runCommand("Centered", "false")
        rm.runCommand("UseNames", "true")

        currentImp.setSlice(sliceNum)
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
        rm.runCommand(currentImp, "Show All with labels")
      }
    })
    logger.info(s"${imp_.getTitle}: $description: Rois: ${pprint.apply(rois)}")
    promptEnterKey()
  }

  def promptEnterKey(): Unit = {
    System.out.println("""Press "ENTER" to continue...""")
    new Scanner(System.in).nextLine
  }
}
