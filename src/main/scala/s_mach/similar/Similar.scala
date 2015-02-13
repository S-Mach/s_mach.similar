package s_mach.similar

import breeze.linalg._
import s_mach.similar.impl.SimilarOps

/**
 * Utility trait for being able to differentiate instances of A's
 *
 * @tparam A type
 */
trait Similar[A] {

  /**
   * Compares two A's for similarity and returns a Double between 0.0 and 1.0
   * A properly written similar(a,b) function should be commutative; that is,
   * similar(a,b) == similar(b,a).
   *
   * @param a1 an A
   * @param a2 another A
   * @return 0.0 if completely dissimilar, 1.0 if equal
   */
  def similar(a1: A, a2: A) : Double


  /**
   * Computes the similarity matrix of 2 vectors such that
   * Matrix(i,j) == similar(ma1(i), ma2(j))
   *
   * @param ma1 Vector of A's
   * @param ma2 Vector of A's
   * @return Similarity Matrix
   */
  def cartesianProduct(
    ma1: IndexedSeq[A],
    ma2: IndexedSeq[A]
  ) : DenseMatrix[Double] =
    SimilarOps.cartesianProduct(ma1,ma1,similar)

  /**
   * Computes the similarity matrix of a vector of A's with itself
   * such that Matrix(i,j) == similar(ma(i), ma(j))
   *
   * @param ma Vector of A's
   * @return Similarity Matrix
   */
  def selfCartesianProduct(
    ma: IndexedSeq[A]
  ) : DenseMatrix[Double] =
    SimilarOps.selfCartesianProduct(ma,similar)
}