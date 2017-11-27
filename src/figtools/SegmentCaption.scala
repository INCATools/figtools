package figtools
import scala.annotation.tailrec
import enumeratum._
import fastparse.all._

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
  def segmentCaption(caption: String): Seq[String] = {
    var segments = ArrayBuffer[String]()

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
        case Parsed.Failure(parser, successIndcex, _) => ()
      }
    }

    // flatten out the labels
    var labelValues = ArrayBuffer[LabelValue.Value]()
    for (label <- labels) {
      for (value <- label.labelType.labelValues.values) {
        value match {
          case LabelValue.Range(start, stop) =>
            if (start.seriesType == stop.seriesType) makeSeries(start, stop)
            else List(start, stop)
          case LabelValue.Value(_,_) => List(value)
        }
      }
    }
    //score the sets
    val sortedLabelValues = labelValues.sortBy(getIndex)
    val labelValues2order = sortedLabelValues.zipWithIndex
    val order2labelValues = labelValues2order.map(_.swap)

    var labelScores = Map[Label, Int]()
    var romanLabelScores = Map[Label, Int]()
    for (i <- 0 until labelValues.size) {
      val labelValue = labelValues(i)
      val next = if (i < labelValues.size-1) Some(labelValues(i+1)) else None
    }

    segments.toList
  }

  def makeSeries(start: LabelValue.Value, stop: LabelValue.Value): Seq[LabelValue.Value] = {
    val startIndex = getIndex(start)
    val stopIndex = getIndex(stop)
    var series = ArrayBuffer[LabelValue.Value]()
    if (startIndex >= 0 && stopIndex >= 0) {
      for (i <- startIndex to stopIndex) {
        series += getValue(i, start.seriesType)
      }
    }
    else {
      series ++= List(start, stop)
    }
    series
  }

  def getIndex(value: LabelValue.Value): Int = {
    value.seriesType match {
      case SeriesType.AlphaLower | SeriesType.AlphaUpper => AlphabeticConverter().convert(value.value)
      case SeriesType.Numeric => value.value.toInt
      case SeriesType.RomanLower | SeriesType.RomanUpper => RomanNumberConverter().convert(value.value).getOrElse(-1)
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
}
