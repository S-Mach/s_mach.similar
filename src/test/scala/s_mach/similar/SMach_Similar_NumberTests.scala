package s_mach.similar

import org.scalatest.{FlatSpec,Matchers}
import breeze.linalg.DenseMatrix
import s_mach.similar.TestObjects._

class SMach_Similar_NumberTests extends FlatSpec with Matchers {
  "selfCartesian" should "1x1 test on numbers" in {
    import s_mach.similar.TestObjects.intSim
    val result= DenseMatrix.ones[Double](1,1)
    Vector(1).selfCartesianProduct should equal(result)
  }

  it should "2x2 test on numbers" in {
    val result= DenseMatrix.ones[Double](2,2)
    result(0,1) = 0.99
    result(1,0) = 0.99
    Vector(1,2).selfCartesianProduct should equal(result)
  }

  it should "ignore empty sequences and give an empty matrix" in {
    Vector[Int]().selfCartesianProduct should equal(DenseMatrix.zeros[Double](0,0))
  }


//

}
