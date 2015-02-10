package s_mach.similar

import org.scalatest.{FlatSpec, Matchers}
import s_mach.similar.relative._

class SMach_Similar_SimilarByFeaturesTest extends FlatSpec with Matchers {
  "SimilarByFeatures" should "compute similarity based on the features of a type" in {
    implicit val s = SimilarByFeatures(Shingler.forChargrams(2 to 3).shingle)
  }
}
