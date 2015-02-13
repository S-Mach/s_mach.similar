package s_mach.similar.impl

object StringDistances {
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
