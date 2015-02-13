package s_mach.similar

import scala.language.higherKinds
import s_mach.similar._
import s_mach.similar.shingle._
import impl.SimilarOps._

// http://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient
object DiceCoefficient {
  implicit def similarTraversable[A,M[AA] <: Traversable[AA]] =
    new Similar[M[A]] {
        override def similar(a1: M[A], a2: M[A]): Double = {
        // TODO: create slightly more efficient function that only computes intersect size
        val (intersectSize,_) = intersectUnionSize(a1,a2)
        (intersectSize.toDouble * 2)/(a1.size + a2.size)
      }
    }

  object ShortString {
    val stringShingler = (_:String).charShingles(2 to 3).toIndexedSeq
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

  object DocString {
    import s_mach.string.WordSplitter.Whitespace

    // TODO: stemming & lemmatization see http://www.thoughtly.co/blog/working-with-text/
    val stringShingler = (_:String).wordShingles(3 to 4).map(_.mkString("|")).toIndexedSeq
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

}
