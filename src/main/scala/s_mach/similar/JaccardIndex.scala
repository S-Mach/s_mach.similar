package s_mach.similar

import scala.language.higherKinds
import impl.SimilarOps._
import impl.ShingleOps._

// http://en.wikipedia.org/wiki/Jaccard_index
object JaccardIndex {
  implicit def similarTraversable[A,M[AA] <: Traversable[AA]] =
    new Similar[M[A]] {
        override def similar(a1: M[A], a2: M[A]): Double = {
        val (intersectSize,unionSize) = intersectUnionSize(a1,a2)
        intersectSize.toDouble/unionSize
      }
    }

  object ShortString {
    val stringShingler = shortStringShingler
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

  object DocString {
    val stringShingler = docStringShingler(s_mach.string.WordSplitter.Whitespace)
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }
}
