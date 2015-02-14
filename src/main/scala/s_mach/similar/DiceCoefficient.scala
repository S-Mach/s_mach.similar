package s_mach.similar

import scala.language.higherKinds
import impl.SimilarOps._
import impl.ShingleOps._

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
    val stringShingler = shortStringShingler
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

  object DocString {
    val stringShingler = docStringShingler(s_mach.string.WordSplitter.Whitespace)
    implicit val stringSimilar = SimilarByFeatures(stringShingler)
  }

}
