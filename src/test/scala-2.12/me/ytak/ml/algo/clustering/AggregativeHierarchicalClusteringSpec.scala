package me.ytak.ml.algo.clustering

import me.ytak.ml.algo.clustering.AggregativeHierarchicalClustering._
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by ytakashina on 2016/10/28.
  */
class AggregativeHierarchicalClusteringSpec extends FlatSpec {

  behavior of "AggregativeHierarchicalClusteringSpec"

  it should "singleLinkage" in {
    val rand = Random
    var xs = Seq[Int]()
    for (i <- 1 to 30) xs = xs :+ rand.nextInt(1000)
    println(xs)
    val c = singleLinkage(xs, (x: Int, y: Int) => math.abs(x - y))
    c.print()
  }
}
