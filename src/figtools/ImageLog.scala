package figtools

import java.util.Scanner

import com.typesafe.scalalogging.Logger
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.Blitter
import ij.{IJ, ImagePlus, WindowManager}
import javax.swing.SwingUtilities

object ImageLog {
  // union type magic
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  val logger = Logger(getClass.getSimpleName)
  def log[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = {
    logger.info(s"${imp_.getTitle}: $description: Rois: ${pprint.apply(rois, height=50)}")
    SwingUtilities.invokeAndWait(()=>{
      val imp = imp_.duplicate()
      // get currently displayed image
      val currentImp = Option(WindowManager.getCurrentImage) match {
        case Some(ci) =>
          // resize canvas if necessary
          if (imp.getWidth > ci.getWidth || imp.getHeight > ci.getHeight) {
            val width = math.max(imp.getWidth, ci.getWidth)
            val height = math.max(imp.getHeight, ci.getHeight)
            IJ.run(ci, "Canvas Size...", s"width=$width height=$height position=Top-Left")
          }
          // make a new slice
          ci.setSlice(ci.getNSlices)
          IJ.run(ci, "Add Slice", "")
          ci.setSlice(ci.getNSlices)
          // copy bits
          ci.getProcessor.copyBits(imp.getProcessor, 0, 0, Blitter.COPY)
          // set the slice label
          ci.getImageStack.setSliceLabel(description, ci.getNSlices)
          ci
        case None =>
          // set the slice label
          imp.getImageStack.setSliceLabel(description, imp.getNSlices)
          imp.show()
          imp
      }
      // add ROIs to new slice
      if (rois.nonEmpty) {
        // get ROI manager and update settings
        val rm = RoiManager.getRoiManager
        rm.runCommand("Associate", "true")
        rm.runCommand("Centered", "false")
        rm.runCommand("UseNames", "true")

        currentImp.setSlice(currentImp.getNSlices)
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
