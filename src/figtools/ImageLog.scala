package figtools

import java.util.Scanner

import com.typesafe.scalalogging.Logger
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.Blitter
import ij.{IJ, ImagePlus, WindowManager}
import javax.swing.SwingUtilities

object ImageLog {
  val logger = Logger(getClass.getSimpleName)
  def log(imp_ : ImagePlus, description: String, rois: Any*): Unit = {
    logger.info(s"${imp_.getTitle}: $description: Rois: ${pprint.apply(rois, height=50)}")
    SwingUtilities.invokeAndWait(()=>{
      val imp = imp_.duplicate()
      // get currently displayed image
      val currentImp = Option(WindowManager.getCurrentImage).getOrElse({
        val i = IJ.createImage(imp.getTitle, "RGB white", imp.getWidth, imp.getHeight, 1)
        i.show()
        i
      })
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
        for ((r,i) <- rois.zipWithIndex) {
          r match {
            case (label: String,roi: Roi) =>
              roi.setName(label)
              rm.addRoi(roi)
            case roi: Roi =>
              roi.setName(s"Roi${i+1}")
              rm.addRoi(roi)
            case _ => throw new RuntimeException(s"Could not parse ROI argument: $r")
          }
        }
        rm.runCommand(currentImp, "Show All with labels")
      }
      currentImp.changes = false
    })
    //promptEnterKey()
  }

  def promptEnterKey(): Unit = {
    System.out.println("""Press "ENTER" to continue...""")
    new Scanner(System.in).nextLine
  }
}
