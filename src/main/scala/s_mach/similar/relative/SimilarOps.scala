package s_mach.similar.relative

import scala.util.Random

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





  /**
   * Creates a Similar[A] based on a Shingler[A,S]
   *
   * @param shingler
   * @tparam A
   * @tparam S
   * @return Similar[A] that uses the jaccard index on shingle sets to compute
   *         similarity
   */
  def simByShingler[A, S](shingler : Shingler[A, S]) : Similar[A] =
    new Similar[A] {

      override def similar(a1: A, a2: A): Double = {
        val shingleA = shingler.shingle(a1).toSet
        val shingleB = shingler.shingle(a2).toSet

        (shingleA intersect shingleB).size.toDouble /
          (shingleA union shingleB).size
      }

    }

}
