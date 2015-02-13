package s_mach.similar

/**
 * A class for computing the similarity between two instances of a type by 
 * comparing the similarity of the identifying features that describe each
 * instance.
 * @param features a function to compute the identifying features of an instance
 * @tparam A the type that can be described in terms of identifying features
 * @tparam F the type that represents the identifying features of A
 */
case class SimilarByFeatures[A,F:Similar](features: A => F) extends Similar[A] {
  override def similar(a1: A, a2: A): Double = {
    features(a1) similar features(a2)
  }

  // TODO: figure out CanMapValues typeclass
//  override def cartesianProduct(
//    ma1: DenseVector[A],
//    ma2: DenseVector[A]
//  ): DenseMatrix[Double] = {
//    ma1.map(features) cartesianProduct ma2.map(features)
//  }
//
//
//  override def selfCartesianProduct(
//    ma: DenseVector[A]
//  ): DenseMatrix[Double] = {
//    ma.map(features).selfCartesianProduct
//  }
}