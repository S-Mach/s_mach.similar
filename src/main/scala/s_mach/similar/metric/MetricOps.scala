package s_mach.similar.metric

import breeze.linalg.DenseVector

/**
 * Created by Gustavo on 2/2/15.
 */
object MetricOps {

  def metricByCartesianDistance[A](p : A => DenseVector[Double]) : Metric[A] = {
    new Metric[A] {
      override def distance(a1: A, a2: A): DenseVector[Double] = {
        position(a2) - position(a1)
      }

      override def position(a1: A): DenseVector[Double] = p(a1)
    }
  }

}
