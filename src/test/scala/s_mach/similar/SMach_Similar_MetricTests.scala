package s_mach.similar

import breeze.linalg.DenseVector
import org.scalatest.{Matchers, FlatSpec}
import s_mach.similar.TestObjects._
/**
 * Created by Gustavo on 2/4/15.
 */
class SMach_Similar_MetricTests extends FlatSpec with Matchers {

  "metricByCartesianDistance" should "behave exactly like geometry" in {
    DenseVector[Double](1,2,3).distance(DenseVector[Double](1,2,3)) should equal (DenseVector[Double](0.0,0.0,0.0))
  }

}
