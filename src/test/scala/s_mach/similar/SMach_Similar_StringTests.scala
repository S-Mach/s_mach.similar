package s_mach.similar
import org.scalatest.{FlatSpec,Matchers}
import breeze.linalg.DenseMatrix
import s_mach.similar.TestObjects._

/**
 * Created by Gustavo on 1/27/15.
 */
class SMach_Similar_StringTests extends FlatSpec with Matchers {
    it should "1x1 test on strings" in {
      Vector("Hello").selfCartesianProduct should equal(DenseMatrix.ones[Double](1,1))
    }

    it should "2x2 test on strings" in {
//      implicit val abc = SimilarOps.simByDistanceThreshold[String](1,SimilarOps.levenshteinDistance)
      val result = DenseMatrix.ones[Double](2,2)
      result(0,1) = 0.2
      result(1,0) = 0.2
      Vector("Hello", "Hi").selfCartesianProduct should equal(result)
    }
}
