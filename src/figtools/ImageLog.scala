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
import com.github.davidmoten.rtree.geometry.Geometries
import de.sciss.equal.Implicits._
import javax.swing.SwingUtilities
import figtools.FigTools.IJ
import javax.imageio.ImageIO
import org.htmlcleaner.{HtmlCleaner, PrettyHtmlSerializer}
import scribe.writer.Writer
import scalatags.Text.all._

import scala.collection.mutable.ArrayBuffer

class ImageLog(name: String = "FigTools", debug: Boolean = false) extends scribe.Logger() {
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

  // union type magic
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  val preLog = new ArrayBuffer[String]()
  case class Report
  (imp: ImagePlus,
   description: String,
   rois: Seq[(String,Roi)] = new ArrayBuffer(),
   log: ArrayBuffer[String] = new ArrayBuffer())
  val reportRecords = ArrayBuffer[Report]()


  scribe.Logger.root.clearHandlers().clearModifiers().withHandler(
    minimumLevel=Some(if (debug) Level.Debug else Level.Info),
    writer = new Writer {
      override def write[M](record: LogRecord[M], output: String) = {
        Console.err.print(output)
        if (reportRecords.size > 1) {
          reportRecords.last.log += output
        }
        else if (reportRecords.size == 1) {
          reportRecords.last.log ++= (preLog ++ Seq(output))
        }
        else {
          preLog += output
        }
      }
    }).replace()

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

  def toHtml(id: String, title: String): String = {
    val Width = 640
    Console.err.println(s"reportRecords=${pp(reportRecords)}")
    val tag = html(
      head(
        link(rel:="stylesheet", attr("type"):="text/css", href:="https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.css"),
        link(rel:="stylesheet", attr("type"):="text/css", href:="https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick-theme.css"),
      ),
      body(
        div(style:="position: relative; width: 80%; margin: auto; text-align: center",
          h1(s"$id - $title"),
          div(attr("class"):=s"id_$id",
            style:="position: relative; padding: 30px; margin: auto; text-align: center",
            reportRecords.zipWithIndex.map{case (r,i)=>
              val baos = new ByteArrayOutputStream()
              val imp = r.imp.duplicate()
              val scaleFactor = Width.toDouble / imp.getWidth.toDouble
              imp.setProcessor(imp.getTitle(), imp.getProcessor.resize(
                (imp.getWidth.toDouble * scaleFactor).toInt,
                (imp.getHeight.toDouble * scaleFactor).toInt))
              ImageIO.write(r.imp.getBufferedImage, "jpg", baos)
              val mapName = s"map-${r.imp.getTitle.replaceAll("""[\/ :_]+""",".")}-step$i"
              div(
                h2(s"${r.imp.getTitle}: Step ${i+1} - ${r.description}"),
                map(attr("name"):=mapName,
                  r.rois.map{r=>(r._1, Geometries.rectangle(
                    r._2.getXBase,
                    r._2.getYBase,
                    r._2.getXBase+r._2.getFloatWidth,
                    r._2.getYBase+r._2.getFloatHeight))}.
                    sortWith{(a,b)=>
                      if ( // a contains b, prefer b
                        a._2.x1 <= b._2.x1 &&
                          a._2.x2 >= b._2.x2 &&
                          a._2.y1 <= b._2.y1 &&
                          a._2.y2 >= b._2.y2) false
                      else if ( // b contains a, prefer a
                        b._2.x1 <= a._2.x1 &&
                          b._2.x2 >= a._2.x2 &&
                          b._2.y1 <= a._2.y1 &&
                          b._2.y2 >= a._2.y2) true
                      else if ( // a intersects b, and the overlap area / total area of a is less than that of b, prefer b
                        a._2.intersects(b._2) &&
                          (((math.min(a._2.x2, b._2.x2)-math.max(a._2.x1, b._2.x1))*(math.min(a._2.y2, b._2.y2)-math.max(a._2.y1, b._2.y1))).toDouble / a._2.area.toDouble) <
                            (((math.min(a._2.x2, b._2.x2)-math.max(a._2.x1, b._2.x1))*(math.min(a._2.y2, b._2.y2)-math.max(a._2.y1, b._2.y1))).toDouble / b._2.area.toDouble)) false
                      // prefer a if a is smaller than b
                      else a._2.area < b._2.area
                    }.map{roi=>
                    area(
                      attr("shape"):="rect",
                      attr("title"):=roi._1,
                      attr("coords"):=Seq(
                        (roi._2.x1 * scaleFactor).toInt,
                        (roi._2.y1 * scaleFactor).toInt,
                        (roi._2.x2 * scaleFactor).toInt,
                        (roi._2.y2 * scaleFactor).toInt,
                      ).mkString(","),
                    )
                  }
                ),
                img(
                  src:=s"data:image/jpg;base64,${Base64.getMimeEncoder(72, Array('\n'.toByte)).encodeToString(baos.toByteArray)}",
                  width:=s"${Width}px",
                  style:="border: 1px solid black",
                  attr("usemap") := s"#$mapName"),
                div(style:="text-align: left; max-height: 450px; overflow-y: scroll; white-space: pre-wrap;",
                  pre(AnsiToHtml.ansiToHtml(s"${r.log.mkString("\n")}"))))
            },
          ),
          script(src:="https://code.jquery.com/jquery-3.3.1.min.js"),
          script(src:="https://cdnjs.cloudflare.com/ajax/libs/maphilight/1.4.0/jquery.maphilight.js"),
          script(src:="https:///cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.min.js"),
          script(src:="https://unpkg.com/infinite-scroll@3/dist/infinite-scroll.pkgd.min.js"),
          script(raw(
            s"""
               |$$(document).ready(function() {
               |  $$('img[usemap]').maphilight({alwaysOn: true});
               |  $$('.id_$id').slick({dots: true, infinite: false, initialSlide: ${reportRecords.size-1}});
               |});
          """.stripMargin)),
        )
      )
    )
    val cleaner = new HtmlCleaner()
    val cleaned = cleaner.clean(s"<!DOCTYPE html>${tag.render}")
    val pretty = new PrettyHtmlSerializer(cleaner.getProperties, "  ")
    pretty.getAsString(cleaned)
  }
  def toHtml(fileName: String, id: String, title: String): Unit = {
    Files.write(Paths.get(fileName), this.toHtml(id, title).getBytes)
  }
}
object ImageLog {
  def apply(name: String = "FigTools", debug: Boolean = false): ImageLog = new ImageLog(name, debug)
}
