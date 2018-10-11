package figtools

import com.github.davidmoten.rtree.{Entries, RTree}
import com.github.davidmoten.rtree.geometry.Rectangle
import figtools.AnalyzeImage.{AnalysisResults, LabelResult}
import org.scalatest.FunSuite

import collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import better.files._

class AnalyzeSuite extends FunSuite {
  val MinOverlapPct = 0.85
  val TestDir = file"./testdir"
  val Url = "http://api.figshare.com/v1"

  def compareResults(expected: AnalysisResults, actual: AnalysisResults): Boolean = {
    if (expected.keySet !== actual.keySet) return false
    for ((id, eir) <- expected) {
      val air = actual(id)
      if (eir.keySet !== air.keySet) return false
      for ((imageFile, elr) <- eir) {
        val alr = air(imageFile)
        if (elr.keySet !== alr.keySet) return false
        for ((label, el) <- elr) {
          val al = alr(label)
          if (el.descriptions.toSet !== al.descriptions.toSet) return false
          var rtree = RTree.create[Int,Rectangle].add(
            al.rois.zipWithIndex.map{case (r,i)=>Entries.entry(i, r.toRect)}.asJava)

          for (roi <- el.rois) {
            val overlaps = rtree.search(roi.toRect).toBlocking.getIterator.asScala.filter{o=>
              o.geometry().intersectionArea(roi.toRect) / o.geometry().area() >= MinOverlapPct &&
              roi.toRect.intersectionArea(o.geometry()) / roi.toRect.area() >= MinOverlapPct
            }.toSeq
            if (overlaps.isEmpty) return false
            val overlap = overlaps.maxBy{o=>o.geometry().intersectionArea(roi.toRect)}
            rtree = rtree.delete(overlap, false)
          }
          if (!rtree.isEmpty) return false
        }
      }
    }
    true
  }

  test("ID 1535748") {
    assert(compareResults(
      new AnalyzeImage(dir=TestDir, ids=Seq("1535748"), url=Some(Url)).analyze(),
      Map(
      "1535748" -> Map(
        "Fig_3.tif" -> Map(
          "A" -> LabelResult(
            ArrayBuffer(
              "(A) Trajectories of the first three principal components (PC1-3) indicate a clear odor separation by the population activity of the 100 AL-units. PC1, PC2 and PC3 explained 43%, 11% and 6% of the variation, respectively. The spontaneous activity before odor onset is shown in grey (Pre-stim). The three second of stimulation are plotted in an odor dependent color code (cp. caption). Note that the population activity for all odors settled in a \u2018fixed point\u2019 at around one second after odor onset (cp. PC1-3 time resolved; right three panels), which is separated from the spontaneous activity. The population activity in response to farnesol shows the most separated trajectory, which is also illustrated separately for each of the first three PCs on the right. "
            ),
            ArrayBuffer(Box(0, 0, 2024, 1963))
          ),
          "B" -> LabelResult(
            ArrayBuffer(
              "(B) The factor loadings (left panel) were used to rank the recorded single units with respect to their contribution to the variation in PC1 starting with the most contributing units at the top. The color coded mean firing rate of the single units illustrates that PC1 contrasted units being excited (positive loadings) by the odor stimuli from a few units being inhibited (negative loadings). Furthermore, the colored matrix suggests a spatial code, e.g. unit 5, 14 and 20 were responding only to farnesol and none of the other stimuli. To further analyze odor induced activity, we set a threshold and extracted all units showing factor loadings >0.05."
            ),
            List()
          )
        )
      )
    )))
  }

  test("ID 3872106") {
    assert(compareResults(
      new AnalyzeImage(dir=TestDir, ids=Seq("3872106"), url=Some(Url)).analyze(),
      Map(
        "3872106" -> Map(
          "Fig 4.TIF" -> Map(
            "a" -> LabelResult(
              ArrayBuffer("(a): \u03b3ECS activity in the roots; "),
              ArrayBuffer(Box(17, 10, 2177, 1262))
            ),
            "b" -> LabelResult(
              ArrayBuffer("(b): GSHS activity in the roots; "),
              ArrayBuffer(Box(11, 1261, 2171, 2548))
            ),
            "c" -> LabelResult(
              ArrayBuffer("(c): \u03b3ECS activity in the leaves; "),
              ArrayBuffer(Box(2178, 10, 4290, 1260))
            ),
            "d" -> LabelResult(
              ArrayBuffer("(d): GSHS activity in the leaves. (For details see legend of Table 1)."),
              ArrayBuffer(Box(2172, 1261, 4290, 2548))
            )
          )
        )
      )
    ))
  }

}
