package me.ytak.ml.algo.clustering

import java.io.PrintStream

import me.ytak.ml.algo.clustering.Cluster.{Couple, Single}

/**
  * Created by ytakashina on 10/22/2016.
  */
sealed abstract class Cluster[+T] {
  val size: Int

  final def extract[U >: T](numberOfGroups: Int): Option[Set[Cluster[U]]] = {
    if (this.size < numberOfGroups) return None
    var xs = Set[Cluster[U]](this)
    for (i <- 1 until numberOfGroups) {
      val current = xs.maxBy(_.size)
      current match {
        case c: Couple[U] =>
          xs += c.left
          xs += c.right
          xs -= c
      }
    }
    Some(xs)
  }

  final def print[U >: T](indent: String = "", out: PrintStream = System.out): Unit = this match {
    case s: Single[U] =>
      out.println(indent + "[ " + s.value + " ]")
    case c: Couple[U] =>
      c.left.print(indent + "|-", out)
      c.right.print(indent.replaceAll("\\-|\\+", " ") + "+-", out)
  }
}

object Cluster {

  case class Single[T] private[Cluster](value: T) extends Cluster {
    final val size = 1
  }

  case class Couple[T] private[Cluster](left: Cluster[T], right: Cluster[T]) extends Cluster {
    final val size = left.size + right.size
  }

  def apply[T](value: T): Single[T] = Single[T](value)

  def apply[T](left: Cluster[T], right: Cluster[T]): Couple[T] = Couple[T](left, right)

  def apply[T](pair: (Cluster[T], Cluster[T])): Couple[T] = Couple[T](pair._1, pair._2)

  def distanceFromSingleToCluster[T](from: Single[T], to: Cluster[T], metrics: (T, T) => Double): Double = to match {
    case t: Single[T] => metrics(from.value, t.value)
    case t: Couple[T] =>
      val left = distanceFromSingleToCluster(from, t.left, metrics)
      val right = distanceFromSingleToCluster(from, t.right, metrics)
      if (left < right) left else right
  }

  def distanceFromClusterToCluster[T](from: Cluster[T], to: Cluster[T], metrics: (T, T) => Double): Double = from match {
    case f: Single[T] => distanceFromSingleToCluster[T](f, to, metrics)
    case f: Couple[T] =>
      val left = distanceFromClusterToCluster(f.left, to, metrics)
      val right = distanceFromClusterToCluster(f.right, to, metrics)
      if (left < right) left else right
  }
}

