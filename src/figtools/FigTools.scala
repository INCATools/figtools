package figtools
import java.util.Properties

import better.files._
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scopt.OptionParser

import collection.JavaConverters._
import edu.stanford.nlp.util.logging.RedwoodConfiguration

object FigTools {
  implicit val formats = org.json4s.DefaultFormats

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
            text("Resolution to use when exporting PDFs to images")
        )
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

  def treeToJSON(tree: Tree): JValue = {
    JArray(List(JString(tree.label().toString()))) ++
      JArray(tree.children().map(t=>if (t.isLeaf) JString(t.value()) else treeToJSON(t)).toList)
  }

  def analyze(config: Config): Unit = {
    // turn off CoreNLP logging
    RedwoodConfiguration.current.clear.apply()
    val dir = "." / "."
    for (datapackage <- dir.listRecursively.filter(_.name == "datapackage.json")) {
      val json = parse(datapackage.contentAsString)
      val description_nohtml = (json \ "description_nohtml").extract[String]
      println(s"file=$datapackage")
      println(s"description_nohtml=$description_nohtml")

      val props = new Properties()
      props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
      val pipeline = new StanfordCoreNLP(props)
      val document = new Annotation(description_nohtml)
      pipeline.annotate(document)
      val sentences = document.get(classOf[SentencesAnnotation])

      for (sentence <- sentences.asScala) {
        val tree = sentence.get(classOf[TreeAnnotation])
        val json = treeToJSON(tree)
        println(s"sentence=${sentence.toString}")
        println(s"tree=${pretty(json)}")
      }
    }
  }
}
