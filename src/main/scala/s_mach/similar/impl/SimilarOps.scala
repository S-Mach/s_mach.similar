package s_mach.similar.impl


import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag
import breeze.linalg._
import s_mach.similar._

object SimilarOps {

  /**
   * Creates a Similar[A] that calculates similarity based on concepts
   * of distance, maximum distance, and the ratio of the distance to the
   * max distance. Helper functions are needed to define what "maximum distance"
   * and "distance" are for a particular type as well as the definition of "empty"
   * This version is for subclasses of AnyRef.
   *
   * If something is empty, it has similary 0.0 to anything else.
   * @param maxDistance Computes the maximum distance between 2 As
   * @param dist Computes the distance between As
   * @param isEmpty Determines if an A is empty
   * @tparam A An A
   * @return Similar[A] for similarity comparison
   */
  def simByDistanceThresholdRefs[A <: AnyRef](maxDistance : (A,A) => Double,
                                          dist : (A,A) => Double,
                                          isEmpty : A => Boolean): Similar[A] = {
    new Similar[A] {
      override def similar(a1: A, a2: A): Double = {
        if (a1 eq a2) {
          1.0
        } else if (isEmpty(a1) || isEmpty(a2)) {
          // TODO: what if they are both empty?
          // TODO: also, if dist can empty a1 & a2 does this short circuit really help?
          0.0
        } else {
          val max = maxDistance(a1, a2)
          (max - dist(a1, a2)) / max
        }
      }
    }
  }

  /**
   * Creates a Similar[A] that calculates similarity based on concepts
   * of distance, maximum distance, and the ratio of the distance to the
   * max distance. Helper functions are needed to define what "maximum distance"
   * and "distance" are for a particular type.
   * This version is for subclasses of AnyVal.
   *
   * If something is empty, it has similary 0.0 to anything else.
   * @param maxDistance Computes the maximum distance between 2 As
   * @param dist Computes the distance between As
   * @tparam A An A
   * @return Similar[A] for similarity comparison
   */
  def simByDistanceThresholdVals[A <: AnyVal](maxDistance : Double,
                                          dist : (A,A) => Double): Similar[A] = {
    new Similar[A] {
      override def similar(a1: A, a2: A): Double = {
        if (a1 == a2) {
          1.0
        } else {
          dist(a1, a2) match {
            case 0 => 1.0
            case within if within <= maxDistance => (maxDistance - within.toDouble) / maxDistance
            case _ => 0
          }
        }
      }
    }
  }

  def simString(dist : (String,String) => Double,
                maxDistance : (String, String) => Double = (_,_) => Double.MaxValue) = {
    simByDistanceThresholdRefs[String](
      (a, b) => Math.max(a.length, b.length),
      dist,
      _.isEmpty)
  }

  def intersectUnionSize[A](lhs: TraversableOnce[A], rhs: TraversableOnce[A]) : (Int,Int) = {
    class Total(mod: Int) {
      var balance = mod
    }
    val lookup = mutable.Map.empty[A,Total]
    var intersect = 0
    var symDiff = 0
    lhs.foreach { a =>
      lookup.get(a) match {
        case Some(total) =>
          total.balance = total.balance + 1
        case None =>
          lookup.put(a,new Total(1))
      }
    }
    rhs.foreach { a =>
      lookup.get(a) match {
        case Some(total) =>
          if(total.balance > 1) {
            total.balance = total.balance - 1
            intersect = intersect + 1
          } else {
            intersect = intersect + 1
            lookup.remove(a)
          }
        case None =>
          symDiff = symDiff + 1
      }
    }
    lookup.valuesIterator.foreach { total =>
      symDiff += total.balance
    }
    val union = intersect + symDiff
    (intersect, union)
  }

  implicit def similarTraversableOnce[A,M[AA] <: TraversableOnce[AA]] =
    new Similar[M[A]] {
        override def similar(a1: M[A], a2: M[A]): Double = {
        val (intersectSize,unionSize) = intersectUnionSize(a1,a2)
        intersectSize.toDouble/unionSize
      }
    }

  def cartesianProduct[A](
    ma1: DenseVector[A],
    ma2: DenseVector[A],
    similar: (A,A) => Double
  ) : DenseMatrix[Double] = {
    val matrix = new DenseMatrix[Double](ma1.length, ma2.length)
    for {
      r <- 0 until ma1.length
      c <- 0 to (r * ma2.length / ma1.length)
    } {
        matrix(r,c) = similar(ma1(r), ma2(c))
        matrix(c,r) = matrix(r,c)
    }
    matrix
  }

  def selfCartesianProduct[A](
    ma: DenseVector[A],
    similar: (A,A) => Double
  ) : DenseMatrix[Double] = {
    val matrix = new DenseMatrix[Double](ma.length, ma.length)
    for {
      r <- 0 until ma.length
      c <- 0 to r
    } {
      if(r == c) matrix(r,c) = 1.0 else {
        matrix(r,c) = similar(ma(r), ma(c))
        matrix(c,r) = matrix(r,c)
      }
    }
    matrix
  }

  def simCentroid[A](
    self: DenseVector[A]
  )(implicit
    similar : Similar[A],
    aClassTag : ClassTag[A]
  ) : A =
    self(
      sum(self.selfCartesianProduct, Axis._1)
        .toArray
        .zipWithIndex
        .maxBy{ case (value, index) => value}._2
    )

  def simGroupBy[A,K](
    self: DenseVector[A],
    threshold: Double
  )(
    f: A => K
  )(implicit
    similar:Similar[K]
  ) : Map[K, DenseVector[A]] = {
      ???
      //def similarValueExists(k : K, seq : IndexedSeq[K]) : Boolean = seq.map(s.similar(k,_)).exists(_ > threshhold)
//      val ks = self.map(f)
//      self.map(a => (a,f(a))).toMap
//        .filter{case (a,k) => similarValueExists(k,ks)}
//        .groupBy(_._2)
//        .mapValues(as => as.map(_._1).toIndexedSeq)
    } //http://stackoverflow.com/questions/2338282/elegant-way-to-invert-a-map-in-scala
      //not sure what I just did here
      //function returns a map of K to Seq[A] generated by taking the A's that map to
      //very similar K's (determined by the threshold value) and grouping them together

}
