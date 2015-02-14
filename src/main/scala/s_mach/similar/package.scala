package s_mach

import scala.reflect.ClassTag
import breeze.linalg._
import s_mach.similar.impl.{SimilarOps, MetricOps}

package object similar {

  implicit class SMach_Similar_PimpMyIndexedSeq[A](
    val self: IndexedSeq[A]
  ) extends AnyVal {
    def toDenseVector(implicit
      aClassTag: ClassTag[A]
    ): DenseVector[A] = DenseVector[A](self.toArray)

    def selfCartesianProduct(implicit
      similar: Similar[A]
    ): DenseMatrix[Double] =
      similar.selfCartesianProduct(self)

    def cartesianProduct(
      other: IndexedSeq[A]
    )(implicit
      similar: Similar[A]
    ): DenseMatrix[Double] =
      similar.cartesianProduct(self, other)

   /**
     * Returns the object most similar to all other objects
     * @param similar Similar of A's
     * @return A most similar to all other A's
     */
    def simCentroid(implicit
      similar: Similar[A]
    ): A = SimilarOps.simCentroid(self)

    def simGroupBy[K](
      threshold: Double
    )(
      f: A => K
    )(implicit
      similar: Similar[K]
    ): Map[K, IndexedSeq[A]] = SimilarOps.simGroupBy(self, threshold)(f)

    def metricCentroid(implicit
      metric: Metric[A],
      aClassTag: ClassTag[A]
    ): A = MetricOps.metricCentroid(self)
  }

  implicit class SMach_Similar_PimpMyDenseVector[A](
    val self: DenseVector[A]
  ) extends AnyVal {


  }

  implicit class SMach_Similar_PimpEverything[A](val self: A) extends AnyVal {

    def similar(rhs: A)(implicit canSimilar: Similar[A]): Double = {
      canSimilar.similar(self, rhs)
    }

    def ==~(rhs: A)(implicit canSimilar: Similar[A]): Double = {
      canSimilar.similar(self, rhs)
    }

    def distance(rhs: A)(implicit metric: Metric[A]) = {
      metric.distance(self, rhs)
    }

    def position(implicit metric: Metric[A]) = {
      metric position self
    }

  }
}