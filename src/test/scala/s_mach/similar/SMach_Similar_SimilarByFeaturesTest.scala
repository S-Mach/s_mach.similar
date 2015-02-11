package s_mach.similar

import org.scalatest.{FlatSpec, Matchers}
import s_mach.similar.relative._

class SMach_Similar_SimilarByFeaturesTest extends FlatSpec with Matchers {
  "SimilarByFeatures with Shingler" should "compute similarity based on the shingle set" in {
    implicit val s = SimilarByFeatures(Shingler.forChargrams(2 to 3).shingle)
    "ham sandwich" similar "ham sandwich" should equal(1.0)
    "turkey sandwich" similar "ham sandwich" should (be >= 0.45 and be <= 0.46)
    "fried chicken" similar "ham sandwich" should be < 0.05
    "dark coco" similar "ham sandwich" should equal(0.0)
    "" similar "ham sandwich" should equal(0.0)
  }

  "SimilarByFeatures with Shingler" should "compute similarity commmutatively" in {
    implicit val s = SimilarByFeatures(Shingler.forChargrams(2 to 3).shingle)
    "turkey sandwich" similar "ham sandwich" should equal("ham sandwich" similar "turkey sandwich")
    "fried chicken" similar "ham sandwich" should equal("ham sandwich" similar "fried chicken")
    "dark coco" similar "ham sandwich" should equal("ham sandwich" similar "dark coco")
    "" similar "ham sandwich" should equal("" similar "ham sandwich")
  }
}
