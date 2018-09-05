package figtools

import com.typesafe.scalalogging.Logger
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.Blitter
import ij.{IJ, ImagePlus, WindowManager}
import javax.swing.SwingUtilities
import org.jline.terminal.TerminalBuilder
import better.files._
import de.sciss.equal.Implicits._

case class ImageLog(showLog: Boolean = true) {
  // union type magic
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  val logger = Logger(getClass.getSimpleName)

  def apply[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = log(imp_, description, step=false, rois: _*)
  def log[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = log(imp_, description, step=false, rois: _*)
  def step[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = log(imp_, description, step=true, rois: _*)

  def log[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, step: Boolean, rois: R*): Unit = {
    if (showLog) {
      logger.info(s"${imp_.getTitle}: $description: Rois.size: ${rois.size}")
      SwingUtilities.invokeAndWait(()=>{
        val imp = imp_.duplicate()
        // get ROI manager and update settings
        val rm = RoiManager.getRoiManager
        rm.runCommand("Associate", "true")
        rm.runCommand("Centered", "false")
        rm.runCommand("UseNames", "true")

        // get currently displayed image
        val currentImp = Option(WindowManager.getCurrentImage) match {
          case Some(ci) =>
            // resize canvas if necessary
            if (imp.getWidth > ci.getWidth || imp.getHeight > ci.getHeight) {
              val width = math.max(imp.getWidth, ci.getWidth)
              val height = math.max(imp.getHeight, ci.getHeight)
              IJ.run(ci, "Canvas Size...", s"width=$width height=$height position=Top-Left")
            }
            // set to last slice
            ci.setSlice(ci.getNSlices)
            if (step) {
              rm.setSelectedIndexes(
                (0 until rm.getCount).filter{i=>
                  rm.getRoi(i).getZPosition === ci.getNSlices}.toArray)
              rm.runCommand("Delete")
              IJ.run(ci, "Delete Slice", "")
              ci.setSlice(ci.getNSlices)
            }
            // make a new slice
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
      if (step) pressAnyKey()
    }
  }

  def pressAnyKey(): Unit = {
    System.out.println("Press any key to continue...")
    for (terminal <- TerminalBuilder.builder().jna(true).system(true).build.autoClosed) {
      terminal.enterRawMode
      for (reader <- terminal.reader.autoClosed) {
        reader.read
      }
    }
  }
}
