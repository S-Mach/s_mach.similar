package s_mach.similar



import s_mach.similar.TestObjects._
import org.scalatest.{FlatSpec,Matchers}
import breeze.linalg.DenseMatrix
import s_mach.similar.relative.Shingler
import s_mach.similar.relative.SimilarOps._

class SMach_Similar_NumberTests extends FlatSpec with Matchers {

  "simByDistanceThreshold" should "create a Similar of AnyVal types" in {
    val doubleSim = simByDistanceThresholdVals[Double](10, (a:Double, b:Double) => Math.abs(a - b))
    doubleSim.similar(1.0, 1.0) should equal(1.0)
    doubleSim.similar(1.0, 2.0) should equal(0.9)
    doubleSim.similar(0, 11) should equal(0.0)
  }

}
