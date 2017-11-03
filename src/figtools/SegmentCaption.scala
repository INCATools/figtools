package figtools
import scala.annotation.tailrec
import enumeratum._
import fastparse.all._
import fastparse.core.Mutable.Success

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object SegmentCaption {
  object Parser {
    val upper = P(CharIn('A' to 'Z'))
    val lower = P(CharIn('a' to 'z'))
    val digits = P(CharIn('0' to '9'))
    val romanUpper = P(CharIn(RomanNumberConverter.toArabic.keys.toList))
    val romanLower = P(CharIn(RomanNumberConverter.toArabic.keys.map(_.toLower).toList))
    val whitespace = P(CharIn(Seq(' ', '\n', '\t', '\r')))
    val label = P((
      upper.rep(min = 1, max = 2).!.map(l=>SeriesType.AlphaUpper(l.mkString(""))) |
      lower.rep(min = 1, max = 2).!.map(l=>SeriesType.AlphaLower(l.mkString(""))) |
      digits.rep(min = 1, max = 2).!.map(l=>SeriesType.Numeric(l.mkString("")))) ~ ".".?)
    val rangeDelim = P("-" | (whitespace.rep(1) ~ StringInIgnoreCase("to", "through", "until") ~ whitespace.rep(1)))
    val labelRange = P(whitespace.rep ~ (label ~ (rangeDelim ~ label).?).map{
      case (start: SeriesType, stop: Option[SeriesType])=>
        stop match {
          case Some(stop) => LabelValue.Range(LabelValue.Value(start), LabelValue.Value(stop))
          case None => LabelValue.Value(start)
        }
    })
    val labelRangeSep = P(CharIn(Seq(',', ';')) ~ whitespace.rep)
    val panelDescription = P(StringInIgnoreCase("panel", "figure").! ~ whitespace.rep(1))
    val labelRanges = P((panelDescription.? ~ labelRange ~ (labelRangeSep ~ labelRange).rep).map{
      case (panelDescription: Option[String], labelRange: LabelValue, labelRanges: Seq[LabelValue]) =>
        LabelValues.Values(labelRange +: labelRanges, panelDescription)
    })
    val paren = P((labelRanges ~ ")").map(LabelType.HalfOpen(_, ParensType.Round)))
    val parens = P(("(" ~ labelRanges ~ ")").map(LabelType.Closed(_, ParensType.Round)))
    val blockParen = P((labelRanges ~ "]").map(LabelType.HalfOpen(_, ParensType.Square)))
    val blockParens = P(("[" ~ labelRanges ~ "]").map(LabelType.Closed(_, ParensType.Square)))
    val curlyParen = P((labelRanges ~ "}").map(LabelType.HalfOpen(_, ParensType.Curly)))
    val curlyParens = P(("{" ~ labelRanges ~ "}").map(LabelType.Closed(_, ParensType.Curly)))
    val labels = P(parens | blockParens | curlyParens | paren | blockParen | curlyParen | labelRanges)
  }

  def segmentCaption(caption: String): List[String] = {
    var segments = ArrayBuffer[String]()
    for (m <- """\([^\)]*?\)|\[[^\]]*?\]|\{[^\}]*?\}|(^|(?<=[.;,:-]\s{1,4}))[^\(\[\{\s]\S*[\]\}\).]""".r.findAllMatchIn(caption)) {
      val result = Parser.labels.parse(m.group(0))
      //Console.println(s"""match="$m", result=$result""")
      result match {
        case Parsed.Success(value, successIndex) => {
          Console.println(s"value=$value")
        }
        case Parsed.Failure(parser, successIndcex, _) => ()
      }
    }
    segments.toList
  }

  object RomanNumberConverter {
    val toArabic = Map('I'->1, 'V'->5, 'X'->10, 'L'->50, 'C'->100, 'D'->500, 'M'->1000)
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
  }

  case class Label(labelType: LabelType,
                   regexMatch: Regex.Match,
                   delim: String)

  sealed abstract class LabelValue extends EnumEntry
  object LabelValue extends Enum[LabelValue] {
    val values = findValues
    case class Value(value: SeriesType) extends LabelValue
    case class Range(start: Value, stop: Value) extends LabelValue
  }

  sealed trait ParensType extends EnumEntry
  object ParensType extends Enum[ParensType] {
    val values = findValues
    case object Round extends ParensType
    case object Square extends ParensType
    case object Curly extends ParensType
    case object Angle extends ParensType
  }

  sealed abstract class LabelValues extends EnumEntry
  object LabelValues extends Enum[LabelValues] {
    val values = findValues
    case class Values(values: Seq[LabelValue], description: Option[String]=None) extends LabelValues
  }

  sealed abstract class LabelType extends EnumEntry
  object LabelType extends Enum[LabelType] {
    val values = findValues
    case class Closed(labelValues: LabelValues, parensType: ParensType) extends LabelType
    case class HalfOpen(labelValues: LabelValues, parensType: ParensType) extends LabelType
    case class Open(labelValues: LabelValues, parensType: ParensType) extends LabelType
  }

  sealed abstract class SeriesType extends EnumEntry
  object SeriesType extends Enum[SeriesType] {
    val values = findValues
    case class Numeric(value: String) extends SeriesType
    case class RomanLower(value: String) extends SeriesType
    case class RomanUpper(value: String) extends SeriesType
    case class AlphaLower(value: String) extends SeriesType
    case class AlphaUpper(value: String)extends SeriesType
  }
}
