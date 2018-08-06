package figtools
import scala.annotation.tailrec
import enumeratum._
import fastparse.all._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import com.typesafe.scalalogging.Logger

import scala.collection.immutable
import de.sciss.equal.Implicits._

object CaptionSegmenter {
  val logger = Logger(getClass.getSimpleName)
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

  def segmentCaption(caption: String): Seq[CaptionGroup] = {
    // Label Parser Grammar
    val upper = P(CharIn('A' to 'Z'))
    val lower = P(CharIn('a' to 'z'))
    val digits = P(CharIn('0' to '9'))
    //val romanUpper = P(CharIn(RomanNumberConverter.toArabic.keys.toList))
    //val romanLower = P(CharIn(RomanNumberConverter.toArabic.keys.map(_.toLower).toList))
    val whitespace = P(CharIn(Seq(' ', '\n', '\t', '\r')))
    val label = P((
      upper.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.AlphaUpper)) |
        lower.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.AlphaLower)) |
        digits.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.Numeric))) ~ ".".?)
    val rangeDelim = P("-" | (whitespace.rep(1) ~ StringInIgnoreCase("to", "through", "until") ~ whitespace.rep(1)))
    val labelRange = P(whitespace.rep ~ (label ~ (rangeDelim ~ label).?).map{
      case (start: LabelValue.Value, stop: Option[LabelValue.Value])=>
        stop match {
          case Some(s) => LabelValue.Range(start, s)
          case None => start
        }
    })
    val labelRangeSep = P(CharIn(Seq(',', ';')) ~ whitespace.rep)
    val panelDescription = P(StringInIgnoreCase("panel", "figure").! ~ whitespace.rep(1))
    val labelRanges = P((panelDescription.? ~ labelRange ~ (labelRangeSep ~ labelRange).rep).map{
      case (panelDescription: Option[String], labelRange: LabelValue, labelRanges: Seq[LabelValue]) =>
        LabelValues(labelRange +: labelRanges, panelDescription)
    })
    val paren = P((labelRanges ~ ")").map(LabelType(_, ParensType.Round, EnclosureType.HalfOpen)))
    val parens = P(("(" ~ labelRanges ~ ")").map(LabelType(_, ParensType.Round, EnclosureType.Closed)))
    val blockParen = P((labelRanges ~ "]").map(LabelType(_, ParensType.Square, EnclosureType.HalfOpen)))
    val blockParens = P(("[" ~ labelRanges ~ "]").map(LabelType(_, ParensType.Square, EnclosureType.Closed)))
    val curlyParen = P((labelRanges ~ "}").map(LabelType(_, ParensType.Curly, EnclosureType.HalfOpen)))
    val curlyParens = P(("{" ~ labelRanges ~ "}").map(LabelType(_, ParensType.Curly, EnclosureType.Closed)))
    val open = P(labelRanges.map(LabelType(_, ParensType.None, EnclosureType.Open)))
    val labelParser = P(parens | blockParens | curlyParens | paren | blockParen | curlyParen | open)

    // parse the caption labels
    var labels = ArrayBuffer[Label]()
    for (m <- """\([^\)]*?\)|\[[^\]]*?\]|\{[^\}]*?\}|(^|(?<=[.;,:-]\s{1,4}))[^\(\[\{\s]\S*[\]\}\).]""".r.findAllMatchIn(caption)) {
      val result = labelParser.parse(m.group(0))
      result match {
        case Parsed.Success(value, _) =>
          val label = Label(value, m, labels.length)
          labels += label
          logger.debug(s"label=${pp(label)}")
        case Parsed.Failure(_, _, _) => ()
      }
    }

    // flatten out the labels
    val labelValues = ArrayBuffer[(LabelValue.Value, Label)]()
    val label2Values = mutable.Map[Label,Seq[LabelValue.Value]]()
    for ((label, i) <- labels.zipWithIndex) {
      val labelVals = ArrayBuffer[LabelValue.Value]()
      for (value <- label.labelType.labelValues.values) {
        val lv = value match {
          case LabelValue.Range(start, stop) =>
            if (start.seriesType === stop.seriesType)
              makeSeries(start, stop)
            else Seq(start, stop)
          case v@LabelValue.Value(_,_) => Seq(v)
        }
        labelVals ++= lv
        labelValues ++= lv.map(l=>(l, label))
      }
      label2Values(label) = labelVals
    }
    logger.debug(s"labelValues=${pp(labelValues)}")
    logger.debug(s"label2Values=${pp(label2Values)}")

    //score the sets
    var series = ArrayBuffer[Series]()
    for (seriesType <- SeriesType.values) {
      logger.debug(s"testing seriesType ${pp(seriesType)}")

      val sortedLabelValues = labelValues.sortBy(x =>(
        getIndex(x._1.value, seriesType),
        x._2.labelIndex))
      logger.debug(s"sortedLabelValues=${pp(sortedLabelValues)}")

      var score = 0
      for (i <- 0 until sortedLabelValues.size-1) {
        val (labelValue, _) = sortedLabelValues(i)
        val (next, _) = sortedLabelValues(i+1)
        for {index <- getIndex(labelValue.value, seriesType)} {
          if (i === 0 && (index !== 0)) score -= index*5
          for {nextIndex <- getIndex(next.value, seriesType)} {
            if (index+1 == nextIndex) score += 5
          }
        }
      }
      var includedLabels = mutable.Set[Label]()
      for (i <- labelValues.indices) {
        val (labelValue, label) = labelValues(i)
        for {index <- getIndex(labelValue.value, seriesType)} {
          includedLabels += label
          score += 1
          if (i < labelValues.size-1) {
            val (next, _) = labelValues(i+1)
            for {nextIndex <- getIndex(next.value, seriesType)} {
              if (index+1 === nextIndex) score += 10
            }
          }
        }
      }
      series += Series(seriesType, score, includedLabels.toList.sortBy(_.labelIndex))
    }
    logger.debug(s"series=${pp(series)}")

    // Try to determine if the description precedes or follows the label caption
    // TODO: improve this?
    val findAfter = labels.lastOption.flatMap(l=>"""^\s*$""".r.findFirstMatchIn(caption.slice(l.regexMatch.end, caption.length))).isEmpty

    val seenLabels = mutable.Set[Label]()
    val captionGroups = ArrayBuffer[CaptionGroup]()
    for (set <- series.sortBy(_.score).reverse) {
      val captions = ArrayBuffer[Caption]()
      val usedLabels = mutable.Set[Label]()
      for ((label, i) <- set.labels.zipWithIndex) {
        if (!seenLabels.contains(label)) {
          seenLabels += label
          usedLabels += label
          val labelValues = label2Values(label).map(_.value)
          val labelIndexes = label2Values(label).flatMap(l=>getIndex(l.value, set.seriesType))

          val description = if (findAfter) {
            val next = if (i < set.labels.size - 1) set.labels(i + 1).regexMatch.start else caption.length
            caption.slice(label.regexMatch.start, next)
          } else {
            val prev = if (i > 0) set.labels(i - 1).regexMatch.end else 0
            caption.slice(prev, label.regexMatch.end)
          }
          captions += Caption(labelValues, labelIndexes, description)
        }
      }
      if (captions.nonEmpty && set.labels.nonEmpty) {
        captionGroups += CaptionGroup(
          captions,
          set.score * (usedLabels.size.toDouble / set.labels.length.toDouble))
      }
    }
    logger.debug(s"caption=$caption")
    logger.debug(s"captionGroups=${pp(captionGroups)}")
    captionGroups
  }

  def makeSeries(start: LabelValue.Value, stop: LabelValue.Value): Seq[LabelValue.Value] = {
    val startIndex = getIndex(start.value, start.seriesType)
    val stopIndex = getIndex(stop.value, stop.seriesType)
    var series = ArrayBuffer[LabelValue.Value]()
    if (startIndex.isDefined && stopIndex.isDefined) {
      for {startIndex <- startIndex
           stopIndex <- stopIndex
           i <- startIndex to stopIndex}
      {
        series += getValue(i, start.seriesType)
      }
    }
    else {
      series ++= immutable.List(start, stop)
    }
    series
  }

  def getIndex(value: String, seriesType: SeriesType): Option[Int] = {
    seriesType match {
      case SeriesType.AlphaLower if """^[a-z]+$""".r.findFirstIn(value).isDefined =>
        Some(AlphabeticConverter().convert(value))
      case SeriesType.AlphaUpper if """^[A-Z]+$""".r.findFirstIn(value).isDefined =>
        Some(AlphabeticConverter().convert(value))
      case SeriesType.Numeric if """^[0-9]+$""".r.findFirstIn(value).isDefined =>
        Some(value.toInt)
      case SeriesType.RomanLower if """^[ivxlcdm]+$""".r.findFirstIn(value).isDefined =>
        RomanNumberConverter().convert(value)
      case SeriesType.RomanUpper if """^[IVXLCDM]+$""".r.findFirstIn(value).isDefined =>
        RomanNumberConverter().convert(value)
      case _ => None
    }
  }

  def getValue(i: Int, seriesType: SeriesType): LabelValue.Value = {
    val value = seriesType match {
      case SeriesType.AlphaUpper => AlphabeticConverter().toUpper(i)
      case SeriesType.AlphaLower => AlphabeticConverter().toLower(i)
      case SeriesType.RomanUpper => RomanNumberConverter().toUpper(i)
      case SeriesType.RomanLower => RomanNumberConverter().toLower(i)
      case SeriesType.Numeric => i.toString
    }
    LabelValue.Value(value, seriesType)
  }

  object RomanNumberConverter {
    val toArabic = Map('I'->1, 'V'->5, 'X'->10, 'L'->50, 'C'->100, 'D'->500, 'M'->1000)
    val digits = immutable.List(
      ("M", 1000),("CM", 900), ("D", 500), ("CD", 400), ("C", 100), ("XC", 90),
      ("L", 50), ("XL",40), ("X", 10), ("IX", 9), ("V", 5), ("IV", 4), ("I", 1))

  }
  case class RomanNumberConverter() {
    def convert(romanNumber: String): Option[Int] = {
      @tailrec def convert(rn: StringBuilder, lastDecimal: Int, acc: Int): Option[Int] = {
        if (rn.isEmpty) Some(acc)
        else {
          val thisDecimal = RomanNumberConverter.toArabic.get(rn.head) match {
            case Some(td) => td
            case None => return None
          }
          if (thisDecimal > lastDecimal) convert(rn.tail, thisDecimal, acc+thisDecimal-lastDecimal-lastDecimal)
          else convert(rn.tail, thisDecimal, acc+thisDecimal)
        }
      }
      val sb = new StringBuilder(romanNumber.toUpperCase)
      val nextDecimal = RomanNumberConverter.toArabic.get(sb.head) match {
        case Some(nd) => nd
        case None => return None
      }
      convert(sb.tail, nextDecimal, nextDecimal)
    }

    def toUpper(index: Int, digits: immutable.List[(String,Int)] = RomanNumberConverter.digits): String = {
      digits match {
        case Nil => ""
        case h :: t => h._1 * (index / h._2) + toUpper(index % h._2, t)
      }
    }
    def toLower(index: Int): String = {
      toUpper(index).toLowerCase
    }
  }

  case class AlphabeticConverter() {
    def convert(alphaNumber: String): Int = {
      alphaNumber.toLowerCase.reverse.zipWithIndex.
        flatMap{case(c,i)=>
          if ("""^[a-z]$""".r.findFirstIn(c.toString).isDefined)
            Some((c.toInt - 'a'.toInt) + (26*i))
          else None}.
        sum+1
    }
    def toUpper(index: Int): String = {
      var result = ""
      var i = index
      while (i > 0) {
        result += 'A'+((i-1) % 26)
        i /= 26
      }
      result.reverse
    }
    def toLower(index: Int): String = {
      toUpper(index).toLowerCase
    }
  }

  case class Label(labelType: LabelType,
                   regexMatch: Regex.Match,
                   labelIndex: Int)

  sealed abstract class LabelValue extends EnumEntry
  object LabelValue extends Enum[LabelValue] {
    val values = findValues
    case class Value(value: String, seriesType: SeriesType) extends LabelValue
    case class Range(startValue: Value, endValue: Value) extends LabelValue
  }

  sealed trait ParensType extends EnumEntry
  object ParensType extends Enum[ParensType] {
    val values = findValues
    case object Round extends ParensType
    case object Square extends ParensType
    case object Curly extends ParensType
    case object Angle extends ParensType
    case object None extends ParensType
  }

  sealed trait EnclosureType extends EnumEntry
  object EnclosureType extends Enum[EnclosureType] {
    val values = findValues
    case object Closed extends EnclosureType
    case object HalfOpen extends EnclosureType
    case object Open extends EnclosureType
  }

  case class LabelValues(values: Seq[LabelValue],
                         description: Option[String]=None)

  case class LabelType(labelValues: LabelValues,
                       parensType: ParensType,
                       enclosureType: EnclosureType)

  sealed trait SeriesType extends EnumEntry
  object SeriesType extends Enum[SeriesType] {
    val values = findValues
    case object Numeric extends SeriesType
    case object RomanLower extends SeriesType
    case object RomanUpper extends SeriesType
    case object AlphaLower extends SeriesType
    case object AlphaUpper extends SeriesType
  }

  case class Series(seriesType: SeriesType,
                    score: Double,
                    labels: Seq[Label])

  case class Caption(label: Seq[String], index: Seq[Int], description: String)

  case class CaptionGroup(captions: Seq[Caption], score: Double)
}
