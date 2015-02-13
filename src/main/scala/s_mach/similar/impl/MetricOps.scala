package s_mach.similar.impl

import breeze.linalg._
import s_mach.similar.Metric
import scala.reflect.ClassTag

object MetricOps {
  def metricByCartesianDistance[A](p : A => DenseVector[Double]) : Metric[A] = {
    new Metric[A] {
      override def distance(a1: A, a2: A): DenseVector[Double] = {
        position(a2) - position(a1)
      }

      override def position(a1: A): DenseVector[Double] = p(a1)
    }
  }

  def metricCentroid[A](
    self: IndexedSeq[A]
  )(implicit
    metric : Metric[A],
    aClassTag : ClassTag[A]
  ) : A = {
    val positions = self.map(metric.position)
    val centroidPoint : DenseVector[Double] = positions
      .foldLeft(DenseVector.zeros[Double](self.length))(_ + _) :/ self.length.toDouble
    self.foldLeft(
      self(0)
    ){(candidate, next) => {
      if (norm(metric.position(candidate) - centroidPoint) > norm(metric.position(next) - centroidPoint)) {
        next
      } else {
        candidate
      }
    }}
  }


}
