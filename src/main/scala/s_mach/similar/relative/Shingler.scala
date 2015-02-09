package s_mach.similar.relative

/**
 * A trait for a Shingler for a given type that can create a set of "shingles"
 * from an instance that describe a set of identifying features for that
 * instance. Shingles get their name from their typical representation of
 * overlapping sequential grams. For the string "hello", the set of 3-chargram
 * shingles is "hel","ell","llo". For the string "hot black coffee", the set of
 * 2-wordgrams is "hot black","black coffee". The shingle set from one instance
 * can be compared against the shingle set from another instance to provide a
 * method for generically computing the similarity for any type that implements
 * Shingler.
 * @tparam A the type to be shingled
 * @tparam Shingle the type of shingle
 */
trait Shingler[A,Shingle] {
  /** @return the set of shingles that describe the instance */
  def shingle(a: A): ShingleSet[Shingle]
}

object Shingler {
  /**
   * A default Shingler implementation
   * @param k_range the range of the number of sequential grams to shingle
   * @param toGrams a function to convert an instance of A to grams
   * @param gramsSize a function to get the number of grams in an instance of
   *                  Grams
   * @param sliceGrams a function to select a range of sequential grams from an
   *                   instance of Grams
   * @tparam A the type to shingle
   * @tparam Grams the type of the collection of grams
   * @tparam Shingle the type of the shingle
   */
  case class ShinglerImpl[A,Grams,Shingle](
    k_range: Range,
    toGrams: A => Grams,
    gramsSize: Grams => Int,
    sliceGrams: (Grams,Int,Int) => Shingle
  ) extends Shingler[A,Shingle] {
    override def shingle(a: A): ShingleSet[Shingle] = {
      new ShingleSet[Shingle] {
        override def foreach[U](f: Shingle => U): Unit = {
          val grams = toGrams(a)
          val maxIndex = gramsSize(grams) - 1
          val maxEndIndex = maxIndex + 1
          k_range.foreach { k =>
            var i = 0
            while(i <= maxIndex && i + k <= maxEndIndex) {
              f(sliceGrams(grams,i,i+k))
              i = i + 1
            }
          }
        }
      }
    }
  }

  /**
   * Create a shingler using char-grams
   * @param k_range the range of the number of sequential
   * @return
   */
  def forChargrams(k_range: Range) : Shingler[String,String] =
    ShinglerImpl[String,String,String](
      k_range = k_range,
      toGrams = { s => s },
      gramsSize = _.length,
      sliceGrams = (s,i,j) => s.substring(i,j)
    )

  def forWordgrams(
    k_range: Range,
    toWords: String => IndexedSeq[String],
    sep: String = "|"
  ) : Shingler[String,String] = ShinglerImpl[String,IndexedSeq[String],String](
    k_range = k_range,
    toGrams = toWords,
    gramsSize = _.size,
    sliceGrams = (s,i,j) => s.slice(i,j).mkString(sep)
  )
}