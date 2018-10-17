package figtools

import java.awt.GraphicsEnvironment
import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Paths}
import java.util.Base64

import scribe.{Level, LogRecord}
import ij.gui.Roi
import ij.plugin.frame.RoiManager
import ij.process.Blitter
import ij.{ImagePlus, WindowManager}
import org.jline.terminal.TerminalBuilder
import better.files._
import de.sciss.equal.Implicits._
import javax.swing.SwingUtilities
import figtools.FigTools.IJ
import javax.imageio.ImageIO
import org.htmlcleaner.{HtmlCleaner, PrettyHtmlSerializer}
import scribe.writer.Writer
import scalatags.Text.all._

import scala.collection.mutable.ArrayBuffer

class ImageLog(name: String = "FigTools", debug: Boolean = false) extends scribe.Logger() {
  // union type magic
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  val preLog = new StringBuilder()
  case class Report
  (imp: ImagePlus,
   description: String,
   rois: Seq[(String,Roi)] = new ArrayBuffer(),
   log: StringBuilder = new StringBuilder())
  val reportRecords = ArrayBuffer[Report]()

  this.withHandler(
    minimumLevel=Some(if (debug) Level.Debug else Level.Info),
    writer = new Writer {
      override def write[M](record: LogRecord[M], output: String) = {
        Console.err.print(output)
        if (reportRecords.size > 1) {
          reportRecords.last.log ++= output
        }
        else if (reportRecords.size == 1) {
          reportRecords.last.log ++= preLog ++ output
        }
        else {
          preLog ++= output
        }
      }
    })

  def clear(): Unit = {
    preLog.clear()
    reportRecords.clear()
  }

  def step[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = image(imp_, description, step=true, rois: _*)
  def image[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, rois: R*): Unit = image(imp_, description, step=false, rois: _*)

  def image[R : (Roi |∨| (String,Roi))#λ](imp_ : ImagePlus, description: String, step: Boolean, rois_ : R*): Unit = {
    val rois = for ((r,i) <- rois_.zipWithIndex) yield {
      r match {
        case (label: String,roi: Roi) =>
          label->roi
        case roi: Roi =>
          s"Roi${i+1}"->roi
        case _ => throw new RuntimeException(s"Could not parse ROI argument: $r")
      }
    }
    reportRecords += Report(imp_, description, rois)
    if (!GraphicsEnvironment.isHeadless) {
      this.info(s"${imp_.getTitle}: $description: Rois.size: ${rois.size}")
      SwingUtilities.invokeLater(()=>{
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
          rm.runCommand(currentImp, "Show All with labels")
          for (roi <- rois) {
            roi._2.setName(roi._1)
            rm.addRoi(roi._2)
          }
        }
        currentImp.changes = false
      })
      if (step) pressAnyKey()
    }
  }

  def pressAnyKey(message: String = "Press any key to continue..."): Unit = {
    Console.err.println(message)
    for (terminal <- TerminalBuilder.builder().jna(true).system(true).build.autoClosed) {
      terminal.enterRawMode
      for (reader <- terminal.reader.autoClosed) {
        reader.read
      }
    }
  }

  def toHtml(): String = {
    val tag = html(
      head(
        script(src:="https://code.jquery.com/jquery-3.3.1.min.js"),
        script(src:="https://cdnjs.cloudflare.com/ajax/libs/maphilight/1.4.0/jquery.maphilight.js"),
        script(
          """
            |$(document).ready(function() {
            |  $('img[usemap]').maphighlight({alwaysOn: true});
            |});
          """.stripMargin),
      ),
      body(
        reportRecords.zipWithIndex.map{case (r,i)=>
          val baos = new ByteArrayOutputStream()
          ImageIO.write(r.imp.getBufferedImage, "jpg", baos)
          val mapName = s"map-${r.imp.getTitle.replaceAll("""[\/ :_]""",".")}"
          div(
            h2(s"${r.imp.getTitle}: Step ${i+1} - ${r.description}"),
            map(attr("name"):=mapName,
              r.rois.map{roi=>
                area(
                  attr("shape"):="rect",
                  attr("title"):=roi._1,
                  attr("coords"):=Seq(
                    roi._2.getXBase.toInt,
                    roi._2.getYBase.toInt,
                    (roi._2.getXBase+roi._2.getFloatWidth).toInt,
                    (roi._2.getYBase+roi._2.getFloatHeight).toInt,
                  ).mkString(","),
                )
              }
            ),
            img(
              src:=s"data:image/jpg;base64,${Base64.getMimeEncoder.encodeToString(baos.toByteArray)}",
              style:="border: 1px solid black",
              attr("usemap") := s"#$mapName"),
            pre(s"${r.log}"))
        }
      )
    )
    val cleaner = new HtmlCleaner()
    val cleaned = cleaner.clean(s"<!DOCTYPE html>${tag.render}")
    val pretty = new PrettyHtmlSerializer(cleaner.getProperties, "  ")
    pretty.getAsString(cleaned)
  }
  def toHtml(fileName: String): Unit = {
    Files.write(Paths.get(fileName), this.toHtml.getBytes)
  }
}
object ImageLog {
  def apply(name: String = "FigTools", debug: Boolean = false): ImageLog = new ImageLog(name, debug)
}
