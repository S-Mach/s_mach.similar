package s_mach.similar

import scala.language.higherKinds
import s_mach.similar._
import s_mach.similar.shingle._
import impl.SimilarOps._

// http://en.wikipedia.org/wiki/Jaccard_index
object JaccardIndex {
  implicit def similarTraversableOnce[A,M[AA] <: TraversableOnce[AA]] =
    new Similar[M[A]] {
        override def similar(a1: M[A], a2: M[A]): Double = {
        val (intersectSize,unionSize) = intersectUnionSize(a1,a2)
        intersectSize.toDouble/unionSize
      }
    }

  object ShortString {
    val stringShingler = (_:String).charShingles(2 to 3)
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

  object DocString {
    import s_mach.string.WordSplitter.Whitespace

    val stringShingler = (_:String).wordShingles(3 to 4).map(_.mkString("|"))
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }
}
