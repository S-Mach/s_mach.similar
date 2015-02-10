package s_mach.similar.relative

import scala.collection.{IterableViewLike, IterableView}

/**
 * A case class for a lazy computed set of shingles
 * @param underlying the underlying shingles collection (ultimately a Stream)
 * @tparam Shingle the type of shingle
 */
case class ShingleSet[Shingle] private(underlying: Iterable[Shingle]) extends
  IterableView[Shingle,Iterable[Shingle]] with
  IterableViewLike[Shingle,Iterable[Shingle],ShingleSet[Shingle]] {

  override def iterator: Iterator[Shingle] = underlying.iterator

  lazy val shingles = underlying.toSet
  override def toSet[B >: Shingle] = shingles.asInstanceOf[Set[B]]
}

object ShingleSet {
  def apply[Shingle](shingles: Stream[Shingle]) : ShingleSet[Shingle] =
    new ShingleSet(shingles)
  def apply[Shingle](shingles: IterableView[Shingle,_]) : ShingleSet[Shingle] =
    new ShingleSet(shingles)

  implicit def similar_ShingleSet[Shingle] =
    new Similar[ShingleSet[Shingle]] {
      override def similar(a1: ShingleSet[Shingle], a2: ShingleSet[Shingle]): Double = {
        val shingleA = a1.toSet
        val shingleB = a2.toSet

        (shingleA intersect shingleB).size.toDouble /
          (shingleA union shingleB).size
      }
    }
}

