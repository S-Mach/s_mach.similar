package s_mach.similar


// http://en.wikipedia.org/wiki/Locality-sensitive_hashing
trait LocalitySensitiveHash[A,H] {
  def hash(a: A) : H
}

