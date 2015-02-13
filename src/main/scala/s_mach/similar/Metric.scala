package s_mach.similar

import breeze.linalg.DenseVector

/**
 * Defines a metric space of A's
 * http://en.wikipedia.org/wiki/Metric_space
 * @author Gustavo Hidalgo
 */
trait Metric[A] {

  /**
   * Defines a distance between 2 A's.
   * In the most accurate case, all of A's instance data would be included
   * in the distance vector: ie, if A had 5 instance variables then this
   * function should return a DenseVector of length 5 where each value is the
   * corresponding distance of each instance variable.
   *
   * If some instance variables are not important for measuring distance, you
   * may choose to exclude them from calculations.
   *
   * @param a1 An A
   * @param a2 Another A
   * @return The distance between them.
   */
  def distance(a1 : A, a2: A) : DenseVector[Double]

  /**
   * Defines a position in "space" of an A
   * @param a1
   * @return a dense vector
   */
  def position(a1: A) : DenseVector[Double]

}
