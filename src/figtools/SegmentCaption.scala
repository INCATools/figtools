package figtools
import scala.annotation.tailrec
import enumeratum._
import fastparse.all._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object SegmentCaption {
  object Parser {
    private val upper = P(CharIn('A' to 'Z'))
    private val lower = P(CharIn('a' to 'z'))
    private val digits = P(CharIn('0' to '9'))
    private val romanUpper = P(CharIn(RomanNumberConverter.toArabic.keys.toList))
    private val romanLower = P(CharIn(RomanNumberConverter.toArabic.keys.map(_.toLower).toList))
    private val whitespace = P(CharIn(Seq(' ', '\n', '\t', '\r')))
    private val label = P((
      upper.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.AlphaUpper)) |
      lower.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.AlphaLower)) |
      digits.rep(min = 1, max = 2).!.map(l=>LabelValue.Value(l.mkString(""), SeriesType.Numeric))) ~ ".".?)
    private val rangeDelim = P("-" | (whitespace.rep(1) ~ StringInIgnoreCase("to", "through", "until") ~ whitespace.rep(1)))
    private val labelRange = P(whitespace.rep ~ (label ~ (rangeDelim ~ label).?).map{
      case (start: LabelValue.Value, stop: Option[LabelValue.Value])=>
        stop match {
          case Some(stop) => LabelValue.Range(start, stop)
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
    val labels = P(parens | blockParens | curlyParens | paren | blockParen | curlyParen | open)
  }

  // upper alpha
  // lower alpha
  // upper roman
  // lower roman
  // numeric
  def segmentCaption(caption: String): Seq[Caption] = {
    // parse the caption labels
    var labels = ArrayBuffer[Label]()
    for (m <- """\([^\)]*?\)|\[[^\]]*?\]|\{[^\}]*?\}|(^|(?<=[.;,:-]\s{1,4}))[^\(\[\{\s]\S*[\]\}\).]""".r.findAllMatchIn(caption)) {
      val result = Parser.labels.parse(m.group(0))
      //Console.println(s"""match="$m", result=$result""")
      result match {
        case Parsed.Success(value, successIndex) => {
          //Console.println(s"value=$value")
          labels += Label(value, m)
        }
        case Parsed.Failure(parser, successIndex, _) => ()
      }
    }

    // flatten out the labels
    var labelValues = ArrayBuffer[(LabelValue.Value, Label)]()
    val label2LabelValues = mutable.Map[Label,ArrayBuffer[LabelValue.Value]]()
    for (label <- labels) {
      for (value <- label.labelType.labelValues.values) {
        val lv = (value match {
          case LabelValue.Range(start, stop) =>
            if (start.seriesType == stop.seriesType) makeSeries(start, stop).map(l=>(l,label))
            else List((start, label), (stop, label))
          case v@LabelValue.Value(_,_) => List((v,label))
        })
        labelValues ++= lv
        for (llvv <- lv) {
          if (!label2LabelValues.contains(llvv._2)) label2LabelValues(llvv._2) = ArrayBuffer()
          label2LabelValues(llvv._2) += llvv._1
        }
      }
    }
    //score the sets
    val labelIndexes = labelValues.zipWithIndex.toMap
    val sortedLabelValues = labelValues.sortBy(x =>(getIndex(x._1.value, x._1.seriesType), labelIndexes(x)))
    var series = ArrayBuffer[Series]()
    for (seriesType <- SeriesType.values) {
      var score = 0
      for (i <- 0 until sortedLabelValues.size-1) {
        val (labelValue, _) = sortedLabelValues(i)
        val (next, _) = sortedLabelValues(i+1)
        if (labelValue.seriesType == next.seriesType) {
          score += 1
          val index = getIndex(labelValue.value, seriesType)
          val nextIndex = getIndex(next.value, seriesType)
          for {index <- index
               nextIndex <- nextIndex}
          {
            if (index+1 == nextIndex) score += 5
          }
        }
      }
      var includedLabels = ArrayBuffer[Label]()
      for (i <- 0 until labelValues.size-1) {
        val (labelValue, label) = labelValues(i)
        val (next, _) = labelValues(i+1)
        if (labelValue.seriesType == next.seriesType) {
          score += 5
          val index = getIndex(labelValue.value, seriesType)
          val nextIndex = getIndex(next.value, seriesType)
          for {index <- index
               nextIndex <- nextIndex}
          {
            if (index+1 == nextIndex) score += 10
          }
          includedLabels += label
        }
      }
      series += Series(seriesType, score, includedLabels)
    }
    // extract the caption descriptions
    val bestSet = series.sortBy(_.score).lastOption
    var captions = ArrayBuffer[Caption]()
    for (bestSet <- bestSet) {
      var findAfter = true
      for {lastLabel <- bestSet.labels.lastOption
           _ <- """^\s*$""".r.findFirstMatchIn(caption.slice(lastLabel.regexMatch.end, caption.size))}
      {
        findAfter = false
      }

      for (i <- bestSet.labels.indices) {
        val label = bestSet.labels(i)
        val labelValues = label2LabelValues(label).map(_.value)
        val labelIndexes = label2LabelValues(label).flatMap(l=>getIndex(l.value, l.seriesType))
        val description = (findAfter match {
          case true =>
            val next = if (i > 0) bestSet.labels(i + 1).regexMatch.start else caption.size
            caption.slice(label.regexMatch.end, next)
          case false =>
            val prev = if (i < bestSet.labels.size - 1) bestSet.labels(i - 1).regexMatch.end else 0
            caption.slice(prev, label.regexMatch.start)
        })
        captions += Caption(labelValues, labelIndexes, description)
      }
    }
    captions
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
      series ++= List(start, stop)
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
    val digits = List(
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

    def toUpper(index: Int, digits: List[(String,Int)] = RomanNumberConverter.digits): String = {
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
                   regexMatch: Regex.Match)

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
                    score: Int,
                    labels: Seq[Label])

  case class Caption(label: Seq[String], index: Seq[Int], description: String)
}
