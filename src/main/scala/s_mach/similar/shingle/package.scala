package s_mach.similar

import s_mach.string._
import s_mach.similar.impl.ShingleOps
import impl.ShingleOps._

package object shingle {
  type Shingler[A,Shingle] = A => TraversableOnce[Shingle]

  type WeightedShingle[A] = (A,Int)
  type TypedWeightedShingle[A] = (Symbol,A,Int)

  implicit class SMach_Similar_Shingle_PimpMyString(
    val self: String
  ) extends AnyVal {
    def charShingles(
      k_range: Range,
      toLowerCase: Boolean = true
    ) : Iterator[String] =
      ShingleOps.charShingles(
        s = self,
        k_range = k_range,
        toLowerCase = toLowerCase
      )

    def wordShingles(
      k_range: Range,
      toLowerCase: Boolean = true
    )(implicit
      splitter:WordSplitter
    ): Iterator[IndexedSeq[String]] =
      ShingleOps.wordShingles(
        s = self,
        k_range = k_range,
        toLowerCase = toLowerCase
      )

    def ngramShingles[Gram](
      k_range: Range,
      toGrams: String => IndexedSeq[Gram]
    ) : Iterator[IndexedSeq[Gram]] =
      ShingleOps.ngramShingles(
        a = self,
        k_range = k_range,
        toGrams = toGrams
      )

    def ksliding(k_range: Range) : Iterator[String] =
      KSlidingIterator(
        k_range = k_range,
        srcSize = self.length,
        srcSlice = self.substring
      )
  }

  implicit class SMach_Similar_Shingle_PimpMyIndexedSeq[A](
    val self: IndexedSeq[A]
  ) extends AnyVal {
    def ngramShingles[Gram](
      k_range: Range,
      toGrams: IndexedSeq[A] => IndexedSeq[Gram]
    ) : Iterator[IndexedSeq[Gram]] =
      ShingleOps.ngramShingles(
        a = self,
        k_range = k_range,
        toGrams = toGrams
      )

    def ksliding(k_range: Range) : Iterator[IndexedSeq[A]] = {
      KSlidingIterator(
        k_range = k_range,
        srcSize = self.size,
        srcSlice = self.slice
      )
    }
  }
}
