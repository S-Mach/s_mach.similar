package s_mach.similar

import breeze.linalg.DenseMatrix
import s_mach.similar.shingle.Shingler

class SimilarByShingles[A,Shingle](shingler: Shingler[A,Shingle])(implicit
  s:Similar[Traversable[Shingle]]
) extends
  SimilarByFeatures[A,Traversable[Shingle]](shingler) {
  
  // TODO: implement these using algo here http://webcourse.cs.technion.ac.il/236621/Winter2010-2011/ho/WCFiles/tutorial%204%20(Duplicate%20Detection).pdf
  override def cartesianProduct(ma1: IndexedSeq[A], ma2: IndexedSeq[A]): DenseMatrix[Double] = ???

  override def selfCartesianProduct(ma: IndexedSeq[A]): DenseMatrix[Double] = ???
}

object SimilarByShingles {
  def apply[A,Shingle](shingler: Shingler[A,Shingle])(implicit
    s:Similar[Traversable[Shingle]]
  ) : SimilarByShingles[A,Shingle] =
    new SimilarByShingles(shingler)
}