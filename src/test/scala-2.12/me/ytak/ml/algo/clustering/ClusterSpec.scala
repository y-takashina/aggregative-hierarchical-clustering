package me.ytak.ml.algo.clustering

import com.sun.javaws.exceptions.InvalidArgumentException
import org.scalatest.FlatSpec

/**
  * Created by ytakashina on 2016/10/29.
  */
class ClusterSpec extends FlatSpec {

  behavior of "ClusterSpec"

  val single = Cluster(1)
  val cluster = Cluster(Cluster(Cluster(60), Cluster(Cluster(68), Cluster(31))), Cluster(Cluster(99), Cluster(19)))
  val largeCluster = Cluster(Cluster(35), Cluster(Cluster(Cluster(Cluster(77), Cluster(Cluster(34), Cluster(22))), Cluster(Cluster(Cluster(60),
    Cluster(Cluster(68), Cluster(31))), Cluster(Cluster(99), Cluster(21)))), Cluster(Cluster(Cluster(8), Cluster(38)), Cluster(72))))

  "cluster constructor" should "not create an instance of a cluster which has a cluster as its value" in {
    assertThrows[InvalidArgumentException](Cluster(Cluster(5)))
    assertThrows[InvalidArgumentException](Cluster(Cluster(Cluster(3), Cluster(5))))
  }

  it should "distanceFromSingleToCluster" in {
    //val metrics = (a: (Int, Int), b: (Int, Int)) => math.sqrt(math.pow(a._1 - b._1, 2) + math.pow(a._2 - b._2, 2))
    val d = Cluster.distanceFromSingleToCluster(single, cluster, (x: Int, y: Int) => math.abs(x - y))
    assert(d == 18)
  }

  it should "distanceFromClusterToCluster" in {
    val d = Cluster.distanceFromClusterToCluster(largeCluster, cluster, (x: Int, y: Int) => math.abs(x - y))
    assert(d == 2)
  }
}
