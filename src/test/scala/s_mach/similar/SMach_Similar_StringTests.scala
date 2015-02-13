package s_mach.similar
import org.scalatest.{FlatSpec,Matchers}
import breeze.linalg.{DenseVector, DenseMatrix}
import s_mach.similar.TestObjects._
import s_mach.similar.impl.{StringDistances, SimilarOps}
import SimilarOps._

class SMach_Similar_StringTests extends FlatSpec with Matchers {
  "selfCartesianProduct" should "1x1 test on strings" in {
    val hello = Vector("Hello")
    hello.selfCartesianProduct should equal(DenseMatrix.ones[Double](1,1))
  }

  it should "2x2 test on strings" in {
    val result = DenseMatrix.ones[Double](2,2)
    result(0,1) = 0.2
    result(1,0) = 0.2
    Vector("Hello", "Hi").selfCartesianProduct should equal(result)
  }

  it should "synthetic test" in {
    val as =
      (1 to 10)
        .map("a" * _)
        .toVector
    val expectedResult = DenseMatrix.zeros[Double](as.length, as.length)
    for(r <- 0 until expectedResult.rows;
        c <- 0 to r) {
      expectedResult(r,c) = (c.toDouble + 1) / (r + 1)
      expectedResult(c,r) = expectedResult(r,c)
    }
    as.selfCartesianProduct should equal (expectedResult)
  }

  "simByDistance" should "compare Strings via a distance function" in {
    implicit val stringSim = simString(StringDistances.levenshteinDistance)
    gettys1 similar gettys2 should not be (0.0)
  }

  "simCentroid" should "find the element most like others" in {
    Vector("hello", "hi", "hey", "hi", "helo", "hi").simCentroid should equal ("hi")
  }

  "simGroupBy" should "group members most like others" in {
    Vector("hello", "hi", "hey", "hi", "helo", "hi","hell","hlelo")
      .simGroupBy(0.6)(v => v) should equal(
        Map(
          "hey" -> Vector("hey"),
          "hi" -> Vector("hi", "hi", "hi"),
          "hello" -> Vector("hello", "helo", "hell", "hlelo")
        )
      )
  }
}
