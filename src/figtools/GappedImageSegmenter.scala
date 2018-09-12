package figtools

import scribe.Logger
import figtools.ImageSegmenter.ImageSegment
import ij.ImagePlus
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer

import scala.collection.mutable.ArrayBuffer
import com.github.davidmoten.rtree.{Entries, RTree}
import com.github.davidmoten.rtree.geometry.Rectangle
import ij.gui.Roi

import scala.collection.JavaConverters._
import de.sciss.equal.Implicits._

import scala.collection.mutable

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
  val pp = pprint.PPrinter(defaultWidth=40, defaultHeight=Int.MaxValue)

  case class Segment(segment: ImageSegment, group: Int)
  case class SegPair(segment: Segment, nearest: Segment)
}

// TODO:
//   - look at overlapping missing panels to determine proper spacing
//   - check matching border colors
class GappedImageSegmenter()(implicit log: ImageLog) extends ImageSegmenter {
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
    logger.debug(s"mergedSegs=${pprint.apply(mergedSegs, height=1000)}")
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
      logger.debug(s"segContent=$segContent")

      val up = ImageSegmenter.Box(seg.box.x,seg.box.y-seg.box.height,seg.box.x2,seg.box.y-1)
      imp3.setRoi(up.toRoi)
      val upHisto = imp3.getProcessor.getHistogram
      val upContent = upHisto(0).toDouble / segArea
      logger.debug(s"upContent=$upContent")

      val down = ImageSegmenter.Box(seg.box.x,seg.box.y+seg.box.height+1,seg.box.x2,seg.box.y2+seg.box.height)
      imp3.setRoi(down.toRoi)
      val downHisto = imp3.getProcessor.getHistogram
      val downContent = downHisto(0).toDouble / segArea
      logger.debug(s"downContent=$downContent")

      val left = ImageSegmenter.Box(seg.box.x-seg.box.width,seg.box.y,seg.box.x-1,seg.box.y2)
      imp3.setRoi(left.toRoi)
      val leftHisto = imp3.getProcessor.getHistogram
      val leftContent = leftHisto(0).toDouble / segArea
      logger.debug(s"leftContent=$leftContent")

      val right = ImageSegmenter.Box(seg.box.x+seg.box.width+1,seg.box.y,seg.box.x2+seg.box.width,seg.box.y2)
      imp3.setRoi(right.toRoi)
      val rightHisto = imp3.getProcessor.getHistogram
      val rightContent = rightHisto(0).toDouble / segArea
      logger.debug(s"rightContent=$rightContent")

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
    log(imp2, "[GappedImageSegmenter] recover small components, segs",
      segs.map{s=>s.box.toRoi}: _*)
    log(imp2, "[GappedImageSegmenter] recover small components, smaller",
      smaller.map{s=>s.box.toRoi}: _*)
    log(imp2, "[GappedImageSegmenter] recover small components, smallSegs",
      smallSegs.map{s=>s.box.toRoi}: _*)
    val toRecover = segs.zipWithIndex.
      map{case (s,i)=>GappedImageSegmenter.Segment(s, i)} ++
      (smaller ++ smallSegs).zipWithIndex.
        map{case (s,i)=>GappedImageSegmenter.Segment(s, -(i+1))}
    logger.debug(s"Running recover small components step")
    val recovered = recoverSmallComponents(imp2, toRecover)
    logger.debug(s"Finished running recover small components step")

    log(imp2, "[GappedImageSegmenter] recover small components",
      recovered.map{s=>s.segment.box.toRoi}: _*)
    val mergedRecovered = mergeOverlappingSegs(recovered.map{s=>s.segment})
    log(imp2, "[GappedImageSegmenter] recover small components, merged",
      mergedRecovered.map{s=>s.box.toRoi}: _*)
    segs = ArrayBuffer(mergedRecovered: _*)
    segs
  }

  def mergeOverlappingSegs(segments: Seq[ImageSegment], useThreshold: Boolean = true): Seq[ImageSegment] = {
    // start with an empty output r-tree
    var rtree = RTree.create[ImageSegment,Rectangle]()
    // iterate through all entries in the input r-tree
    for (seg <- segments) {
      // look for overlapping segments that overlap by at least MergeThreshold area amount
      val overlaps = rtree.search(seg.box.toRect).toBlocking.getIterator.asScala.filter{overlaps=>
        !useThreshold || {
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
        }
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

  // define case class orderings for the sorted set
  implicit def impOrder: Ordering[ImagePlus] =
    Ordering.by(i => i.asInstanceOf[AnyRef].hashCode)
  implicit def boxOrder: Ordering[ImageSegmenter.Box[Int]] =
    Ordering.by(b => (b.x, b.x2, b.y, b.y2))
  implicit def imageSegOrder: Ordering[ImageSegment] =
    Ordering.by(i => (i.imp, i.box))
  implicit def segOrder: Ordering[Segment] =
    Ordering.by(s => (s.segment, -s.group))
  // sort by distance, then by merged area
  implicit def segPairOrder: Ordering[SegPair] =
    Ordering.by{p =>
      val box = ImageSegmenter.Box(
        math.min(p.segment.segment.box.x, p.nearest.segment.box.x),
        math.min(p.segment.segment.box.y, p.nearest.segment.box.y),
        math.max(p.segment.segment.box.x2, p.nearest.segment.box.x2),
        math.max(p.segment.segment.box.y2, p.nearest.segment.box.y2))
      (p.segment.segment.box.toRect.distance(p.nearest.segment.box.toRect),
        (box.x2-box.x+1) * (box.y2-box.y+1),
        p.segment,
        p.nearest)
    }

  def recoverSmallComponents(imp: ImagePlus, segs: Seq[Segment]): Seq[Segment] = {
    // build the r-tree by merging all the overlapping components
    logger.debug(s"building merged r-tree using ${segs.size} segs")
    var rtree = RTree.create[Segment,Rectangle]()
    for (seg <- segs) {
      val overlaps = rtree.search(seg.segment.box.toRect).
        toBlocking.getIterator.asScala.
        map{overlaps=>
          val intersectionBox = ImageSegmenter.Box(
            math.max(seg.segment.box.x, overlaps.value.segment.box.x),
            math.max(seg.segment.box.y, overlaps.value.segment.box.y),
            math.min(seg.segment.box.x2, overlaps.value.segment.box.x2),
            math.min(seg.segment.box.y2, overlaps.value.segment.box.y2))
          val intersectionArea =
            (intersectionBox.x2 - intersectionBox.x) *
              (intersectionBox.y2 - intersectionBox.y)
          val minBoxArea = math.min(
            (seg.segment.box.x2 - seg.segment.box.x) * (seg.segment.box.y2 - seg.segment.box.y),
            (overlaps.value.segment.box.x2 - overlaps.value.segment.box.x) * (overlaps.value.segment.box.y2 - overlaps.value.segment.box.y))
          (overlaps, intersectionArea, minBoxArea)
        }.
        filter{case (_, intersectionArea, minBoxArea)=>
          intersectionArea.toDouble / minBoxArea.toDouble > MergeThreshold
      }.toSeq

      // build a merged segment
      val merged = Segment(
        ImageSegment(seg.segment.imp,
          ImageSegmenter.Box(
            (Seq(seg.segment.box.x) ++ overlaps.map{_._1.value.segment.box.x}).min,
            (Seq(seg.segment.box.y) ++ overlaps.map{_._1.value.segment.box.y}).min,
            (Seq(seg.segment.box.x2) ++ overlaps.map{_._1.value.segment.box.x2}).max,
            (Seq(seg.segment.box.y2) ++ overlaps.map{_._1.value.segment.box.y2}).max)),
        (Seq(seg.group) ++ overlaps.map{_._1.value.group}).max)
      // remove all the segments that go into the merged segment
      rtree = rtree.delete(overlaps.map{_._1}.asJava, true)
      // add the merged segment
      rtree = rtree.add(Entries.entry(merged, merged.segment.box.toRect))
    }
    logger.debug(s"built merged r-tree with ${rtree.size} elements")

    // set of each segment paired with its nearest neighbor, sorted by minimum distance
    val segpairs = mutable.SortedSet[SegPair](
      rtree.entries.toBlocking.getIterator.asScala.map{_.value}.
      flatMap(entry=>{
        val nearest = rtree.nearest(entry.segment.box.toRect, Double.MaxValue, Int.MaxValue).
          filter{e=>
            (e.value.segment.box !== entry.segment.box) &&
              !(e.value.group >= 0 &&
                entry.group >= 0)}.
          toBlocking.getIterator.asScala.toSeq
        if (nearest.isEmpty) None else Some(SegPair(
          entry,
          nearest.head.value))
      }).toSeq: _*)
    // index of segment -> segpair
    val segIndex = mutable.Map[Segment,SegPair](segpairs.map{p=>p.segment->p}.toSeq: _*)
    // index of neighbor -> segpairs
    val nearestIndex = new mutable.HashMap[Segment, mutable.Set[SegPair]] with mutable.MultiMap[Segment, SegPair]
    segpairs.foreach{p=> nearestIndex.addBinding(p.nearest, p) }
    // index of group ID -> segments in group
    val groups = new mutable.HashMap[Int, mutable.Set[Segment]] with mutable.MultiMap[Int, Segment]
    for (e <- rtree.entries.toBlocking.getIterator.asScala) {
      groups.addBinding(e.value.group, e.value)
    }

    // process all the segpairs
    while (segpairs.nonEmpty) {
      // pop the first pair from the set
      val pair = segpairs.head
      logger.debug(s"pair=${pp(pair)}")
      val segpairsSize = segpairs.size
      segpairs -= pair
      assert(segpairsSize-1 === segpairs.size)
      // figure out the group number
      // negative numbers are mergeable
      val pairsegs = Seq(pair.segment, pair.nearest)
      val group = pairsegs.map{_.group}.max
      logger.debug(s"group=$group")
      // figure out what needs to be re-grouped
      val toGroup = (pairsegs ++
        pairsegs.filter{s=>s.group !== group}.flatMap{s=>groups(s.group)}).toSet
      logger.debug(s"toGroup=${pp(toGroup)}")
      // create the grouped segments
      val grouped = toGroup.map{s=>Segment(s.segment, group)}
      logger.debug(s"grouped=${pp(grouped)}")

      // delete the old segments from rtree, segpairs, segindex, and groups
      val rtreeSize = rtree.size
      rtree = rtree.delete(
        toGroup.map{tg=>Entries.entry(tg, tg.segment.box.toRect)}.asJava,
        true)
      assert(rtreeSize-toGroup.size === rtree.size,
        s"rtreeSize($rtreeSize)-toGroup(${pp(toGroup)}) != rtree.size(${rtree.size})")

      val pairsToRemove = toGroup.flatMap{tg=>segIndex.get(tg)}.toSet - pair
      val intersect = segpairs.intersect(pairsToRemove)
      assert(pairsToRemove.size === intersect.size,
        s"pairsToRemove=${pp(pairsToRemove)}, intersect=${pp(intersect)}")
      val segpairsSize2 = segpairs.size
      segpairs --= pairsToRemove
      assert(segpairsSize2-pairsToRemove.size === segpairs.size)

      val segIndexSize = segIndex.size
      segIndex --= toGroup
      assert(segIndexSize-toGroup.size === segIndex.size)
      toGroup.foreach{tg=>
        groups.removeBinding(tg.group, tg)
      }

      // add the newly grouped segments to rtree, group, segpairs, segIndex, and nearestIndex
      for (g <- grouped) {
        rtree = rtree.add(Entries.entry(g, g.segment.box.toRect))
        groups.addBinding(g.group, g)
        val groupedNearest = rtree.nearest(g.segment.box.toRect, Double.MaxValue, Int.MaxValue).
          filter { e =>
            (e.value.segment.box !== g.segment.box) &&
              (e.value.group !== g.group) &&
              !(e.value.group >= 0 &&
                g.group >= 0)
          }.
          toBlocking.getIterator.asScala.toSeq.headOption
        for (gn <- groupedNearest) {
          // add new merged pair
          val groupedPair = SegPair(g, gn.value)
          segpairs += groupedPair
          segIndex += g -> groupedPair
          nearestIndex.addBinding(gn.value, groupedPair)
        }
      }
      // update nearest neighbors for the new merged segment pair and any existing segment pairs
      for {
        tg <- toGroup
        sps <- nearestIndex.get(tg)
      } {
        nearestIndex -= tg
        for (sp <- sps) {
          if (segpairs.contains(sp)) {
            segpairs -= sp
            val nearests = rtree.nearest(sp.segment.segment.box.toRect, Double.MaxValue, Int.MaxValue).
              filter{e=>
                (e.value.segment.box !== sp.segment.segment.box) &&
                  (e.value.group !== sp.segment.group) &&
                  !(e.value.group >= 0 &&
                    sp.segment.group >= 0)}.
              toBlocking.getIterator.asScala.toSeq.headOption
            for (nearest <- nearests) {
              val updatedSp = SegPair(sp.segment, nearest.value)
              segpairs += updatedSp
              segIndex += sp.segment->updatedSp
              nearestIndex.addBinding(nearest.value, updatedSp)
            }
          }
        }
      }
      //val rois = groups.toSeq.map{case (i,g)=>
      //  s"$i"->g.map{s=>s.segment.box}.reduce((a,b) =>
      //    ImageSegmenter.Box(
      //      Seq(a.x, b.x).min,
      //      Seq(a.y, b.y).min,
      //      Seq(a.x2, b.x2).max,
      //      Seq(a.y2, b.y2).max)
      //  ).toRoi
      //}
      //log.step(imp, s"[GappedImageSegmenter] recover small components step, rois.size=${rois.size}", rois: _*)
    }
    // return the remaining r-tree entries
    logger.debug(s"return merged group entries")
    val mergedSegments = groups.map{case (i,g)=>
      g.reduce((a,b) =>
        Segment(
          ImageSegment(a.segment.imp,
            ImageSegmenter.Box(
              Seq(a.segment.box.x, b.segment.box.x).min,
              Seq(a.segment.box.y, b.segment.box.y).min,
              Seq(a.segment.box.x2, b.segment.box.x2).max,
              Seq(a.segment.box.y2, b.segment.box.y2).max)),
          i)
      )
    }.toSeq
    mergedSegments
  }
}

