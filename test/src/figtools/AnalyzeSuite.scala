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

  test("ID 1463671") {
    assert(compareResults(
      new AnalyzeImage(dir=TestDir, ids=Seq("1463671"), url=Some(Url)).analyze(),
      Map(
        "1463671" -> Map(
          "Fig_4.tif" -> Map(
            "A" -> LabelResult(
              ArrayBuffer(
                "(A) Top row: raw lateral force profiles for the +FF and \u2212FF subgroups (darker and lighter colors, respectively, with experiment 1 in red, experiment 2 in blue, mean \u00b1 SEM). The +FF and \u2212FF subgroups display differently-shaped force profiles but similar overall force levels during training, corresponding to similar average endpoint errors (not shown). The \u2212FF subgroup data are well-captured by the adaptation coefficient measure (black line), which is a regression onto the ideal force profile, while the +FF data are not. This explains why learning and decay appear attenuated in the +FF subgroup in Fig 3. We performed a control experiment consisting of a 0-FF \u201ctraining\u201d block and a zEC retention block (experiment 3) to provide a baseline reference for adaptation and decay. Considering the force profiles (colored traces in top row) relative to this baseline reference (gray traces in top row) reveals symmetric adaptation and decay between +FF and \u2212FF conditions for both training and retention (colored traces in second row). Note that the control experiment force profiles increase substantially during the retention period, indicating that the unreferenced shooting movement decay data in Fig 3 are confounded by a tendency to produce more positive force during an extended EC block. Without control-referencing, this tendency causes the +FF decay to be underestimated and the \u2212FF decay to be overestimated, as it was in Fig 3. Since the control-referenced retention data is still not always well explained by the shape of the adaptation coefficient measure, we quantified adaptation using an integrated lateral force measure that is agnostic to the shape of the force profile. "
              ),
              ArrayBuffer(Box(18, 0, 2169, 752))
            ),
            "B" -> LabelResult(
              ArrayBuffer(
                "(B) Like Fig 3, the control-referenced vEC and zEC learning and decay appear similar (red vs blue), but here we also see symmetric learning and decay across +FF and \u2212FF conditions in both experiments. The strong decay apparent in both the +FF and \u2212FF arms of the vEC experiment is in contrast to reports of the vEC manipulation eliminating decay. "
              ),
              ArrayBuffer(Box(0, 823, 1664, 1484))
            ),
            "C" -> LabelResult(
              ArrayBuffer(
                "(C) Consistent with the unreferenced adaptation coefficients in Fig 3, the analysis based on control-referenced integrated lateral forces shows similar learning and decay for the analogous vEC and zEC experiments, but it displays much greater symmetry across +FF vs \u2212FF conditions. For point-to-point movements, the vEC condition (red) actually seems to increase the decay somewhat over the corresponding zEC data (blue). These data fail to support the prediction that vEC-based retention will reduce or eliminate decay. For point-to-point movements, the hatched bars are experiment 4 (vECopp) in the 90\u00b0 direction, which is opposite to the zEC movement direction. The red and blue solid bars represent experiments 4 and 5 (vEC and zEC) in the 270\u00b0 direction. Error bars show SEM."
              ),
              ArrayBuffer(Box(20, 1524, 2117, 1980))
            )
          )
        )
      )
    ))
  }

}
