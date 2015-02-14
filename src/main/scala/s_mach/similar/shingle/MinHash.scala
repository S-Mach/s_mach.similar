package s_mach.similar.shingle

import s_mach.similar.LocalitySensitiveHash

// TODO: not sure if this is the best abstraction
abstract class MinHash[A,Shingle,H](
  shingler: Shingler[A,Shingle],
  hashers: Iterable[Shingle => H]
)(implicit o:Ordering[H]) extends LocalitySensitiveHash[A,H] {
  def hash(a: A, n: Int) : Iterator[H]
  def hash(a: A) : H = {
    shingler(a).map { shingle =>
      hashers.iterator.map { hash =>
        hash(shingle)
      }.min
    }.min
  }
}
