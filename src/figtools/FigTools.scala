package figtools
import java.awt.Rectangle
import java.nio.{ByteBuffer, FloatBuffer}
import java.util.Properties
import java.util.regex.Pattern

import better.files._
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scopt.OptionParser

import sys.process._
import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration
import ij.{IJ, ImageJ, ImagePlus, WindowManager}
import ij.gui.Roi
import ij.io.Opener
import ij.measure.{Measurements, ResultsTable}
import ij.plugin.filter.ParticleAnalyzer
import ij.plugin.frame.RoiManager
import ij.process.{BinaryProcessor, ByteProcessor, ImageConverter}

import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import net.sourceforge.tess4j.Tesseract
import org.tensorflow._


object FigTools {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  case class Config(mode: String = "",
                    pdfExportResolution: Int = 150)

  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("figtools") {
      override def showUsageOnError = true
      head("figtools", "0.1.0")
      help("help").text("prints this usage text")
      version("version").text("print the program version")
      cmd("analyze").action((_, c) => c.copy(mode = "analyze")).
        text("recursively analyze a nested directory of figures.").
        children(
          opt[Int]("pdf-export-resolution").action((x,c) => c.copy(pdfExportResolution = x)).
            text("Resolution to use when exporting PDFs to images"))
    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        config.mode match {
          case "analyze" => analyze(config)
          case "" => parser.showUsage()
        }
      case None =>
        System.exit(1)
    }
  }

  def treeToJSONList(tree: Tree): JValue = {
    JArray(List(JString(tree.label().toString))) ++
      (if (tree.children().length ==1 && tree.getChild(0).isLeaf)
        JString(tree.getChild(0).value())
      else JArray(tree.children().map(t=>if (t.isLeaf) JString(t.value()) else treeToJSONList(t)).toList))
  }

  def treeToJSONObject(tree: Tree): JValue = {
    JObject(List(tree.label().toString ->
      (if (tree.children().length ==1 && tree.getChild(0).isLeaf)
        JString(tree.getChild(0).value())
      else JArray(tree.children().map(t=>if (t.isLeaf) JString(t.value()) else treeToJSONObject(t)).toList))))
  }

  def analyze(config: Config): Unit = {
    // turn off CoreNLP logging
    RedwoodConfiguration.current.clear.apply()
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    val pipeline = new StanfordCoreNLP(props)

    // start an embedded ImageJ instance
    val imagej = new ImageJ(ImageJ.EMBEDDED)
    imagej.exitWhenQuitting(true)

    // get the ROI Manager instance
    IJ.run("ROI Manager...")
    val roiManager = WindowManager.getFrame("ROI Manager").asInstanceOf[RoiManager]
    ParticleAnalyzer.setRoiManager(roiManager)

    val dir = "." / "."
    for (datapackage <- dir.listRecursively.filter(_.name == "datapackage.json")) {
      val json = parse(datapackage.contentAsString)
      val description_nohtml = (json \ "description_nohtml").extract[String]
      println(s"file=$datapackage")
      println(s"description_nohtml=\n$description_nohtml")

      val document = new Annotation(description_nohtml)
      pipeline.annotate(document)
      val sentences = document.get(classOf[SentencesAnnotation])

      for (sentence <- sentences.asScala) {
        println(s"sentence=${sentence.toString}")
        //val tree = sentence.get(classOf[TreeAnnotation])
        //val json = treeToJSONObject(tree)
        //println(s"tree=\n${tree.pennString()}")
        //println(s"json=\n${pretty(json)}")
      }
      println()

      val resources = (json \ "resources").extract[List[JValue]]
      for (resource <- resources) {
        breakable {
          val name = (resource \ "name").extract[String]
          val imageFile = datapackage.parent / name
          if (!imageFile.toString.matches("""(?i).*\.(png|jpe?g|tiff?|pdf)""")) {
            println(s"Skipping non-image file $imageFile")
            break
          }
          val imageFiles = ArrayBuffer(imageFile)
          if (imageFile.exists) {
            // use the convert command-line tool to convert PDF files to image files
            if (imageFile.extension.isDefined && imageFile.extension.get.toLowerCase == ".pdf") {
              imageFiles.clear()
              val cmd = Seq("convert", "-density", config.pdfExportResolution.toString, imageFile.toString, s"$imageFile.png")
              val status = cmd.!
              if (status != 0) {
                throw new RuntimeException(s"Command $cmd returned exit status $status")
              }
              val pngs = datapackage.parent.glob("*.png")
              val outimages = pngs.filter(f =>
                f.name.toString.matches(raw"""^${Pattern.quote(imageFile.name.toString)}(-[0-9]+)?\.png$$"""))
              imageFiles ++= outimages
            }
          }
          else {
            Console.err.println(s"Could not find file $imageFile")
          }
          if (imageFiles.size > 1) {
            println(s"File $imageFile contains more than one image, skipping")
            break
          }
          if (imageFiles.isEmpty) {
            println(s"File $imageFile contains no images, skipping")
            break
          }
          var imp = new Opener().openImage(imageFiles.head.toString())
          if (imp == null) {
            println(s"Could not open image file $imageFile, skipping")
            break
          }
          new ImageConverter(imp).convertToGray8()
          val binaryProc = new BinaryProcessor(new ByteProcessor(imp.getImage))
          binaryProc.autoThreshold()
          imp = new ImagePlus(imp.getTitle, binaryProc)
          imp.show()
          val rt = new ResultsTable()
          val particleAnalyzer = new ParticleAnalyzer(
              ParticleAnalyzer.SHOW_RESULTS |
              ParticleAnalyzer.SHOW_SUMMARY |
              ParticleAnalyzer.ADD_TO_MANAGER |
              ParticleAnalyzer.EXCLUDE_EDGE_PARTICLES |
              ParticleAnalyzer.CLEAR_WORKSHEET |
              ParticleAnalyzer.IN_SITU_SHOW,
            Measurements.AREA |
              Measurements.MEAN |
              Measurements.MIN_MAX |
              Measurements.RECT,
            rt, 0.0, Double.PositiveInfinity)
          if (!particleAnalyzer.analyze(imp)) {
            throw new RuntimeException("ParticleAnalyzer.analyze() returned false!")
          }

          val bundle = SavedModelBundle.load(
            s"${System.getProperty("user.home")}/src/tensorflow-ocr/savedmodel/",
            "serve")
          val session = bundle.session()

          val WidthLimits = (14, 80)
          val HeightLimits = (14, 80)

          for (i <- (0 until rt.getCounter).reverse) {
            breakable {
              val bx = rt.getValue("BX", i).toInt
              val by = rt.getValue("BY", i).toInt
              val width = rt.getValue("Width", i).toInt
              val height = rt.getValue("Height", i).toInt
              if (!(WidthLimits._1 <= width && width <= WidthLimits._2 &&
                    HeightLimits._1 <= height && height <= HeightLimits._2))
              {
                rt.deleteRow(i)
                roiManager.deselect(roiManager.getRoi(i))
                break
              }

              // use tesseract OCR
              val instance = new Tesseract()
              val bi = imp.getBufferedImage
              val text = instance.doOCR(bi, new Rectangle(bx, by, width, height)).trim
              println(s"OCR text ($bx,$by,$width,$height)='$text'")

              // use tensorflow-ocr
              imp.setRoi(new Roi(bx, by, width, height))
              val cropped = new ImagePlus()
              cropped.setProcessor(imp.getProcessor.crop().resize(24, 24))
              val bytes = cropped.getProcessor.getPixels.asInstanceOf[Array[Byte]]
              //for (y <- 0 until 24) {
              //  for (x <- 0 until 24) {
              //    print(f"${bytes(y*24+x) & 0xFF}%03d ")
              //  }
              //  println()
              //}
              val floats: Array[Float] = bytes.map(x => (1.0 - (2 * (x & 0xFF)) / 255.0).asInstanceOf[Float])
              //for (y <- 0 until 24) {
              //  for (x <- 0 until 24) {
              //    print(f"${floats(y*24+x)}%+1.2f ")
              //  }
              //  println()
              //}
              for {
                inputX <- Tensor.create(Array(24l, 24l), FloatBuffer.wrap(floats)).autoClosed
                dropoutKeepProb <- Tensor.create(Array[Long](), FloatBuffer.wrap(Array(1f))).autoClosed
                trainPhase <- Tensor.create(DataType.BOOL, Array[Long](),
                  ByteBuffer.wrap(Array(0.asInstanceOf[Byte]))).autoClosed
                result <- session.runner().
                  feed("input/input_x", inputX).
                  feed("state/dropout_keep_prob", dropoutKeepProb).
                  feed("state/train_phase", trainPhase).
                  fetch("model/prediction/Dense_96/add").
                  run().get(0).autoClosed
              } {
                val output = result.copyTo(Array.ofDim[Float](1, 96))
                val (prob, best) = output(0).zipWithIndex.maxBy(_._1)
                val ch = (best + 32).toChar
                println(s"Tensorflow detected character '$ch' with prob $prob")
              }
            }
          }
          while (imp.isVisible) {
            Thread.sleep(200)
          }
        }
      }
    }
  }
}
