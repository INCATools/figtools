package figtools

import com.github.davidmoten.rtree.geometry.{Geometries, Rectangle}
import ij.gui.Roi
import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JInt, JObject}

case class Box(x: Int, y: Int, x2: Int, y2: Int) {
  require(x <= x2 && y <= y2, s"Invalid box: $this")
  def width: Int = x2 - x + 1
  def height: Int = y2 - y + 1
  def toRect: Rectangle = Geometries.rectangle(x.toFloat, y.toFloat, x2.toFloat, y2.toFloat)
  def toRoi: Roi = new Roi(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
}

object Box {
  class BoxSerializer extends CustomSerializer[Box](_ => (
    {
      case JObject(
      JField("x", JInt(x)) ::
      JField("y", JInt(y)) ::
      JField("x2", JInt(x2)) ::
      JField("y2", JInt(y2)) :: Nil) =>
        Box(x.toInt, y.toInt, x2.toInt, y2.toInt)
    },
    {
      case box: Box =>
        JObject(
          JField("x", JInt(box.x)) ::
          JField("y", JInt(box.y)) ::
          JField("x2", JInt(box.x2)) ::
          JField("y2", JInt(box.y2)) :: Nil)
    }
  ))
}
