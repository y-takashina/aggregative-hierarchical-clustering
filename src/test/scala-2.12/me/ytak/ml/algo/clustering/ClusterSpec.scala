package me.ytak.ml.algo.clustering

import org.scalatest.FlatSpec

/**
  * Created by ytakashina on 2016/10/29.
  */
class ClusterSpec extends FlatSpec {

  behavior of "ClusterSpec"

  val single = Cluster(1)
  val cluster = Cluster(Cluster(Cluster(60), Cluster(Cluster(68), Cluster(31))), Cluster(Cluster(99), Cluster(19)))
  val largeCluster = Cluster(Cluster(35), Cluster(Cluster(Cluster(Cluster(77), Cluster(Cluster(34), Cluster(22))), Cluster(Cluster(Cluster(60),
    Cluster(Cluster(68), Cluster(31))), Cluster(Cluster(99), Cluster(19)))), Cluster(Cluster(Cluster(8), Cluster(38)), Cluster(72))))

  //"cluster constructor" should "not create an instance of a cluster which has a cluster as its value" in {
  //  assertThrows[InvalidArgumentException](Cluster(Cluster(5)))
  //  assertThrows[InvalidArgumentException](Cluster(Cluster(Cluster(3), Cluster(5))))
  //}

  it should "distanceFromSingleToCluster" in {
    //val metrics = (a: (Int, Int), b: (Int, Int)) => math.sqrt(math.pow(a._1 - b._1, 2) + math.pow(a._2 - b._2, 2))
    //val eval = (x: (Int,  Double), y: (Int, Double)) => if (x._2 < y._2) x._2 else y._2
    val single = Cluster(0)
    val clusters = Cluster(Cluster(5), Cluster(Cluster(3), Cluster(4)))
    val d = Cluster.distanceFromSingleToCluster(single, clusters, (x: Int, y: Int) => math.abs(x - y))
    assert(d == 3)
  }

  it should "distanceFromClusterToCluster" in {

  }

/*
  "extract function" should "extract the cluster to the given number of clusters." in {
    assert(single.extract(1).get(single))
    assert(cluster.extract(3).get(Cluster(60)))
    assert(cluster.extract(3).get(Cluster(Cluster(68), Cluster(31))))
    assert(cluster.extract(3).get(Cluster(Cluster(99), Cluster(19))))
    assert(largeCluster.extract(12).isDefined)
    largeCluster.extract(5).get.foreach(_.print())
  }

  "extract function" should "fail when the number of members of the cluster is smaller than the given number." in {
    assert(single.extract(2).isEmpty)
    assert(cluster.extract(6).isEmpty)
    assert(largeCluster.extract(13).isEmpty)
  }
*/
}
