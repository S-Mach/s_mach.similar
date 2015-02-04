package s_mach.similar.relative

trait Shingler[A,S] {
  def shingle(a: A): List[S]
}

