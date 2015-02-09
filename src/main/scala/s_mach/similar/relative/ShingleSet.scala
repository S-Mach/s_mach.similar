package s_mach.similar.relative

/**
 * A trait for a lazily computed set of shingles that can be compared for
 * similarity
 * @tparam Shingle the shingle type
 */
trait ShingleSet[Shingle] extends Traversable[Shingle] { self =>
  /** @return a new ShingleSet that lazily computes the union of this and other
    * */
  def union(other: ShingleSet[Shingle]) : ShingleSet[Shingle] =
    new ShingleSet[Shingle] {
      override def foreach[U](f: (Shingle) => U) = {
        self.foreach(f)
        other.foreach(f)
      }
    }
}

object ShingleSet {
  implicit def similar_ShingleSet[Shingle] = new Similar[ShingleSet[Shingle]] {
    override def similar(
      a1: ShingleSet[Shingle],
      a2: ShingleSet[Shingle]
    ): Double = {
      val shingleSet1 = a1.toSet
      val shingleSet2 = a2.toSet
      // Jaccard Index
      (shingleSet1 intersect shingleSet2).size.toDouble /
        (shingleSet1 union shingleSet2).size.toDouble
    }
  }
}

