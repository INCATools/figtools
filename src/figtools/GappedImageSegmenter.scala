package figtools

import com.typesafe.scalalogging.Logger
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer

import scala.collection.mutable.ArrayBuffer
import ImageLog.log
import com.github.davidmoten.rtree.{Entries, RTree}
import com.github.davidmoten.rtree.geometry.Rectangle
import ij.gui.Roi

import scala.collection.JavaConverters._

import scala.collection.mutable

// TODO:
// - fix recover small components - make sure it gets added to closest actual ROI, not box
// - recover missing panels
//   - look at overlapping missing panels to determine proper spacing
//   - check matching border colors
class GappedImageSegmenter extends ImageSegmenter {
  import GappedImageSegmenter._
  val logger = Logger(getClass.getSimpleName)

  override def segment(imp: ImagePlus): Seq[ImageSegment] = {
    val imp2 = imp.duplicate()
    //log(imp2, "[GappedImageSegmenter] original image")

    // binarize the image using a threshold of 0.95
    val threshold = (255.0 * BinarizeThreshold).toInt
    val lut = (0 until 256).map{v=>if (v < threshold) 0 else 255.toByte.toInt}.toArray
    imp2.getProcessor.applyTable(lut)
    log(imp2, s"[GappedImageSegmenter] Binarize with $threshold threshold")

    val rt = new ResultsTable
    val particleAnalyzer = new ParticleAnalyzer(
      ParticleAnalyzer.CLEAR_WORKSHEET |
      ParticleAnalyzer.IN_SITU_SHOW,
      Measurements.MIN_MAX |
      Measurements.RECT,
      rt,
      0.0,
      Double.MaxValue)
    if (!particleAnalyzer.analyze(imp2)) throw new RuntimeException(
      "ParticleAnalyzer.analyze() returned false!")
    val particles = (0 until rt.getCounter).map{i=>
      Option(rt.getLabel(i)).getOrElse(s"Roi${i+1}")->
      new Roi(
        rt.getValue("BX", i).toInt,
        rt.getValue("BY", i).toInt,
        rt.getValue("Width", i).toInt,
        rt.getValue("Height", i).toInt)}
    log(imp2, s"[GappedImageSegmenter] Particle Analyzer", particles: _*)

    // Get the objects and iterate through them
    var segs = ArrayBuffer[ImageSegment]()
    val smallSegs = ArrayBuffer[ImageSegment]()
    (0 until rt.getCounter).foreach{i=>
      val bx = rt.getValue("BX", i).toInt
      val by = rt.getValue("BY", i).toInt
      val width = rt.getValue("Width", i).toInt
      val height = rt.getValue("Height", i).toInt

      if (width > imp2.getWidth.toDouble / ParticleThreshold &&
          height > imp2.getHeight.toDouble / ParticleThreshold)
      {
        if ((width*height).toDouble / (imp2.getWidth*imp2.getHeight).toDouble < largeParticleFilter) {
          val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
          val segment = ImageSegment(imp, box)
          segs += segment
        }
      }
      else {
        val box = ImageSegmenter.Box(bx, by, bx+width, by+height)
        val segment = ImageSegment(imp, box)
        smallSegs += segment
      }
    }
    log(imp2,
      s"[GappedImageSegmenter] filter boxes smaller than (${(imp2.getWidth.toDouble / ParticleThreshold).toInt},${(imp2.getHeight.toDouble / ParticleThreshold).toInt})",
      segs.map{_.box.toRoi}: _*)

    val mergedSegs = mergeOverlappingSegs(segs)
    segs = ArrayBuffer(mergedSegs: _*)
    logger.info(s"mergedSegs=${pprint.apply(mergedSegs, height=1000)}")
    log(imp2, "[GappedImageSegmenter] merge overlapping segments", mergedSegs.map{_.box.toRoi}: _*)

    // temporarily eliminate small components
    val (larger, smaller) = segs.sortBy({seg=>
        (seg.box.x2 - seg.box.x) *
        (seg.box.y2 - seg.box.y)
      }).lastOption match
    {
      case Some(largest) =>
        val larger = segs.filter(seg=>
          (seg.box.x2 - seg.box.x).toDouble / (largest.box.x2 - largest.box.x).toDouble > BboxThreshold &&
          (seg.box.y2 - seg.box.y).toDouble / (largest.box.y2 - largest.box.y).toDouble > BboxThreshold)
        val smaller = segs.filter(seg=>
          !((seg.box.x2 - seg.box.x).toDouble / (largest.box.x2 - largest.box.x).toDouble > BboxThreshold &&
            (seg.box.y2 - seg.box.y).toDouble / (largest.box.y2 - largest.box.y).toDouble > BboxThreshold))
        (larger, smaller)
      case None =>
        logger.warn(s"No largest bounding box found!")
        (segs, Seq())
    }
    log(imp2, "[GappedImageSegmenter] temporarily eliminate small components", larger.map{s=>s.box.toRoi}: _*)

    // recover missing panels
    val imp3 = imp2.duplicate()
    val largerRtree = RTree.create[ImageSegment,Rectangle].
      add(larger.map{s=>Entries.entry(s, s.box.toRect)}.asJava)
    val recoverMissing = larger.flatMap{seg =>
      imp3.setRoi(seg.box.toRoi)
      val segArea = ((seg.box.x2 - seg.box.x) * (seg.box.y2 - seg.box.y)).toDouble
      val segHisto = imp3.getProcessor.getHistogram
      val segContent = segHisto(0).toDouble / segArea
      logger.info(s"segContent=$segContent")

      val up = ImageSegmenter.Box(seg.box.x,seg.box.y-seg.box.height,seg.box.x2,seg.box.y-1)
      imp3.setRoi(up.toRoi)
      val upHisto = imp3.getProcessor.getHistogram
      val upContent = upHisto(0).toDouble / segArea
      logger.info(s"upContent=$upContent")

      val down = ImageSegmenter.Box(seg.box.x,seg.box.y+seg.box.height+1,seg.box.x2,seg.box.y2+seg.box.height)
      imp3.setRoi(down.toRoi)
      val downHisto = imp3.getProcessor.getHistogram
      val downContent = downHisto(0).toDouble / segArea
      logger.info(s"downContent=$downContent")

      val left = ImageSegmenter.Box(seg.box.x-seg.box.width,seg.box.y,seg.box.x-1,seg.box.y2)
      imp3.setRoi(left.toRoi)
      val leftHisto = imp3.getProcessor.getHistogram
      val leftContent = leftHisto(0).toDouble / segArea
      logger.info(s"leftContent=$leftContent")

      val right = ImageSegmenter.Box(seg.box.x+seg.box.width+1,seg.box.y,seg.box.x2+seg.box.width,seg.box.y2)
      imp3.setRoi(right.toRoi)
      val rightHisto = imp3.getProcessor.getHistogram
      val rightContent = rightHisto(0).toDouble / segArea
      logger.info(s"rightContent=$rightContent")

      (if (math.abs(segContent-upContent) < ContentDiff &&
        upContent > ContentMin &&
        seg.box.height < seg.box.y &&
        Option(largerRtree.search(up.toRect).toBlocking.firstOrDefault(null)).isDefined)
        Seq(ImageSegment(imp, up))
      else Seq()) ++
        (if (math.abs(segContent-downContent) < ContentDiff &&
          downContent > ContentMin &&
          seg.box.y+seg.box.height*2 < imp3.getHeight &&
          Option(largerRtree.search(down.toRect).toBlocking.firstOrDefault(null)).isDefined)
          Seq(ImageSegment(imp, down))
        else Seq()) ++
        (if (math.abs(segContent-leftContent) < ContentDiff &&
          leftContent > ContentMin &&
          seg.box.width < seg.box.x &&
          Option(largerRtree.search(left.toRect).toBlocking.firstOrDefault(null)).isDefined)
          Seq(ImageSegment(imp, left))
        else Seq()) ++
        (if (math.abs(segContent-rightContent) < ContentDiff &&
          rightContent > ContentMin &&
          seg.box.x+seg.box.width*2 < imp3.getWidth &&
          Option(largerRtree.search(right.toRect).toBlocking.firstOrDefault(null)).isDefined)
          Seq(ImageSegment(imp, right))
        else Seq())
    }
    // TODO: fix recover missing panels code
    //segs = larger ++ recoverMissing
    segs = larger
    //log(imp2, "[GappedImageSegmenter] recover missing panels", recoverMissing.map{s=>s.box.toRoi}: _*)

    //check segmentation area
    val segmentArea = segs.map{s=>(s.box.x2-s.box.x)*(s.box.y2-s.box.y)}.sum
    val imageArea = imp2.getWidth * imp2.getHeight
    if (segmentArea.toDouble / imageArea.toDouble < SegmentAreaCutoff) {
      logger.warn(s"Image segment area is $segmentArea/$imageArea=${segmentArea.toDouble/imageArea.toDouble} is less than $SegmentAreaCutoff, skipping")
      return Seq()
    }

    // run small components recovery step
    val toRecover = segs.map{s=>GappedImageSegmenter.Segment(s, skip=true)} ++
      (smaller ++ smallSegs).map{s=>GappedImageSegmenter.Segment(s,skip=false)}
    logger.info(s"Running recover small components step")
    val recovered = recoverSmallComponents(toRecover)
    logger.info(s"Finished running recover small components step")

    log(imp2, "[GappedImageSegmenter] recover small components",
      recovered.map{s=>s.segment.box.toRoi}: _*)
    val mergedRecovered = mergeOverlappingSegs(recovered.map{s=>s.segment})
    log(imp2, "[GappedImageSegmenter] recover small components, merged",
      mergedRecovered.map{s=>s.box.toRoi}: _*)
    segs = ArrayBuffer(mergedRecovered: _*)
    segs
  }
}

object GappedImageSegmenter {
  val BinarizeThreshold = 0.95
  val ParticleThreshold = 20.0
  val largeParticleFilter = 0.9
  val MergeThreshold = 0.1
  val BboxThreshold = 0.15
  val NewBoxPctCutoff = 0.2
  val SegmentAreaCutoff = 0.4
  val ContentDiff = 0.1
  val ContentMin = 0.01

  def mergeOverlappingSegs(segments: Seq[ImageSegment]): Seq[ImageSegment] = {
    // start with an empty output r-tree
    var rtree = RTree.create[ImageSegment,Rectangle]()
    // iterate through all entries in the input r-tree
    for (seg <- segments) {
      // look for overlapping segments that overlap by at least MergeThreshold area amount
      val overlaps = rtree.search(seg.box.toRect).toBlocking.getIterator.asScala.filter{overlaps=>
        val intersectionBox = ImageSegmenter.Box(
          math.max(seg.box.x, overlaps.value.box.x),
          math.max(seg.box.y, overlaps.value.box.y),
          math.min(seg.box.x2, overlaps.value.box.x2),
          math.min(seg.box.y2, overlaps.value.box.y2))
        val intersectionArea =
          (intersectionBox.x2 - intersectionBox.x) *
            (intersectionBox.y2 - intersectionBox.y)
        val minBoxArea = math.min(
          (seg.box.x2 - seg.box.x) * (seg.box.y2 - seg.box.y),
          (overlaps.value.box.x2 - overlaps.value.box.x) * (overlaps.value.box.y2 - overlaps.value.box.y))
        intersectionArea.toDouble / minBoxArea.toDouble > MergeThreshold
      }.toSeq
      // build a merged segment
      val merged = ImageSegment(seg.imp,
        ImageSegmenter.Box(
          (Seq(seg.box.x) ++ overlaps.map{_.value.box.x}).min,
          (Seq(seg.box.y) ++ overlaps.map{_.value.box.y}).min,
          (Seq(seg.box.x2) ++ overlaps.map{_.value.box.x2}).max,
          (Seq(seg.box.y2) ++ overlaps.map{_.value.box.y2}).max))
      // remove all the segments that go into the merged segment
      rtree = rtree.delete(overlaps.asJava, true)
      // add the merged segment
      rtree = rtree.add(Entries.entry(merged, merged.box.toRect))
    }
    rtree.entries.toBlocking.getIterator.asScala.map{_.value}.toSeq
  }


  case class Segment(segment: ImageSegment, skip: Boolean)
  case class SegPair(segment: Segment, nearest: Segment, distance: Double)
  def recoverSmallComponents(segs: Seq[Segment]): Seq[Segment] = {
    // build the r-tree
    var rtree = RTree.create[Segment,Rectangle].
      add(segs.map{s=>Entries.entry(s, s.segment.box.toRect)}.asJava)
    // Get a sorted set of segment nearest-neighbor pairs
    implicit def orderByDistance: Ordering[SegPair] = Ordering.by{_.distance}
    val segpairs = mutable.SortedSet(
      segs.filter{e=> !e.skip}.
        flatMap(entry=>{
          val nearest = rtree.nearest(entry.segment.box.toRect, Double.MaxValue, 2).
            filter{e=>e.value.segment != entry.segment}.
            toBlocking.getIterator.asScala.toSeq.headOption
          if (nearest.isEmpty) None else Some(SegPair(
            entry,
            nearest.get.value,
            entry.segment.box.toRect.distance(nearest.get.geometry())))
        }): _*)

    // keep an index of segment -> segpair
    val segIndex = mutable.Map[Segment,SegPair](segpairs.map{p=>p.segment->p}.toSeq: _*)
    // keep an index of neighbor -> segpairs
    val nearestIndex = new mutable.HashMap[Segment, mutable.Set[SegPair]] with mutable.MultiMap[Segment, SegPair]
    segpairs.foreach{p=>
      nearestIndex.addBinding(p.nearest, p)
    }
    // process all the segpairs
    while (segpairs.nonEmpty) {
      // pop the first pair from the set
      val pair = segpairs.head
      segpairs -= pair
      // merge the pair's boxes
      val box = ImageSegmenter.Box(
        math.min(pair.segment.segment.box.x, pair.nearest.segment.box.x),
        math.min(pair.segment.segment.box.y, pair.nearest.segment.box.y),
        math.max(pair.segment.segment.box.x2, pair.nearest.segment.box.x2),
        math.max(pair.segment.segment.box.y2, pair.nearest.segment.box.y2))
      // get the set of r-tree entries that will be merged by joining this segpair
      val toMerge = rtree.search(box.toRect).toBlocking.getIterator.asScala.map{_.value}.toSeq
      // create the merged segment
      val mergedSegment = Segment(ImageSegment(pair.segment.segment.imp, box), toMerge.exists{_.skip})
      // remove the r-tree entries that we are mergning
      rtree = rtree.delete(toMerge.map{tm=>Entries.entry(tm, tm.segment.box.toRect)}.asJava, true)
      // add the newly merged r-tree entry
      rtree = rtree.add(Entries.entry(mergedSegment, box.toRect))
      // remove the merged segments from the segpairs set
      segpairs --= toMerge.flatMap{tm=>segIndex.get(tm)}
      segIndex --= toMerge
      // update nearest neighbors for the new merged segment pair and any existing segment pairs
      for {sp <- Seq(SegPair(mergedSegment, mergedSegment, 0)) ++
        toMerge.flatMap { tm =>
          val ni = nearestIndex.get(tm)
          if (ni.nonEmpty) ni.get else Seq()
        }
      } {
        segpairs -= sp
        nearestIndex -= sp.nearest
        rtree.nearest(mergedSegment.segment.box.toRect, Double.MaxValue, 2).
          filter{e=>e.value.segment != mergedSegment.segment}.
          toBlocking.getIterator.asScala.toSeq.headOption.foreach{nearest=>
          val updatedSp = SegPair(
            sp.segment,
            nearest.value,
            sp.segment.segment.box.toRect.distance(nearest.geometry()))
          segpairs += updatedSp
          segIndex += sp.segment->updatedSp
          nearestIndex.addBinding(nearest.value, updatedSp)
        }
      }
    }
    // return the remaining r-tree entries
    rtree.entries.toBlocking.getIterator.asScala.map{_.value}.toSeq
  }

}
