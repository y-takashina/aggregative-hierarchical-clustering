package me.ytak.ml.algo.clustering

import me.ytak.ml.algo.clustering.Cluster._

/**
  * Created by ytakashina on 2016/10/27.
  */
object AggregativeHierarchicalClustering {

  def singleLinkage[T](data: Seq[T], metrics: (T, T) => Double): Cluster[T] = {
    var clusters: Set[Cluster[T]] = data.map(Cluster(_)).toSet

    while (clusters.size != 1) {
      var min = Double.MaxValue
      var nearestPair = (clusters.head, clusters.last)
      for (c1 <- clusters; c2 <- clusters; if c1 != c2) {
        val d = distanceFromClusterToCluster(c1, c2, metrics)
        if (d < min) {
          min = d
          nearestPair = (c1, c2)
        }
      }
      clusters += Cluster(nearestPair)
      clusters -= nearestPair._1
      clusters -= nearestPair._2
    }

    clusters.head
  }
}