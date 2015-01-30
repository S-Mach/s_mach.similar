package s_mach.similar

import s_mach.similar.CanSimilar

object SimilarOps {

  /**
   * Creates a CanSimilar[A] that calculates similarity based on concepts
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
   * @return CanSimilar[A] for similarity comparison
   */
  def simByDistanceThreshold[A <: AnyRef](maxDistance : (A,A) => Int,
                                          dist : (A,A) => Int,
                                          isEmpty : A => Boolean): CanSimilar[A] = {
    new CanSimilar[A] {
      override def similar(a1: A, a2: A): Double = {
        val max = maxDistance(a1, a2)
        if (a1 eq a2) {
          1.0
        } else if (isEmpty(a1) || isEmpty(a2)) {
          0.0
        } else {
          dist(a1, a2) match {
            case 0 => 1.0
            case within if within <= max => (max - within.toDouble) / max
            case _ => 0
          }
        }
      }
    }
  }

  /**
   * Creates a CanSimilar[A] that calculates similarity based on concepts
   * of distance, maximum distance, and the ratio of the distance to the
   * max distance. Helper functions are needed to define what "maximum distance"
   * and "distance" are for a particular type.
   * This version is for subclasses of AnyVal.
   *
   * If something is empty, it has similary 0.0 to anything else.
   * @param maxDistance Computes the maximum distance between 2 As
   * @param dist Computes the distance between As
   * @tparam A An A
   * @return CanSimilar[A] for similarity comparison
   */
  def simByDistanceThreshold[A <: AnyVal](maxDistance : Int,
                                          dist : (A,A) => Int): CanSimilar[A] = {
    new CanSimilar[A] {
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

  // this needs to calculate the union size somehow...
  // http://en.wikipedia.org/wiki/Jaccard_index
  def calcJaccardIndex(a_intersect_b_size: Int, a_size: Int, b_size: Int) : Double = {
    if (a_size == 0 || b_size == 0) 1 else {
      a_intersect_b_size / (a_size + b_size)
    }
  }

  // http://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient
  def calcDiceCoefficient(a_intersect_b_size: Int, a_size: Int, b_size: Int) : Double = {
    if (a_size == 0 || b_size == 0) 1 else {
      a_intersect_b_size / (a_size + b_size)
    }
  }

  // http://en.wikipedia.org/wiki/Levenshtein_distance
  def levenshteinDistance(s1: String, s2: String) : Int = {
    def min(numbers: Int*): Int = numbers.min
    val lenStr1 = s1.length
    val lenStr2 = s2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)
    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1;
         j <- 1 to lenStr2) {
      val cost = if (s1(i - 1) == s2(j-1)) 0 else 1

      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }
    d(lenStr1)(lenStr2)
  }



  // http://en.wikipedia.org/wiki/Hamming_distance
  def hammingDistance(s1: String, s2: String) : Int = {
    if (s1.length != s2.length) throw new IllegalArgumentException("Strings must be of equal length")
    levenshteinDistance(s1, s2)
  }


  // http://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
  def needlemanWunsch(s1: String, s2: String) : Int = ???

  // Implement more algos here: http://web.archive.org/web/20081224234350/http://www.dcs.shef.ac.uk/~sam/stringmetrics.html
}
