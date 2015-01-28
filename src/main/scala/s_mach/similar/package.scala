package s_mach

import scala.reflect.ClassTag
import scala.util.matching.Regex
import breeze.linalg._

package object similar {
  implicit class SMach_Similar_PimpMyIndexedSeq[A](val self: IndexedSeq[A]) extends AnyVal {


    def selfCartesianProduct(implicit s: CanSimilar[A], aClassTag: ClassTag[A]) : DenseMatrix[Double] = {
      s.selfCartesianProduct(Vector[A](self.toArray))
    }

    def cartesianProduct(other : Vector[A])(implicit s:CanSimilar[A], aClassTag: ClassTag[A]) : Matrix[Double] = {
      s.cartesianProduct(Vector[A](self.toArray), other)
    }

    /**
     * Returns the object most similar to all other objects
     * @param s
     * @return
     */
    def centroid(implicit s:CanSimilar[A]) : A = {
      val simMatrix = selfCartesianProduct
      self.zipWithIndex.maxBy[Double]{
        case (_, index:Int) => sum(simMatrix(index, ::).t)
      }._1
    }

    def simGroupBy[K](threshhold: Double)(f: A => K)(implicit s:CanSimilar[K]) : Map[K, IndexedSeq[A]] = {
      def similarValueExists(k : K, seq : IndexedSeq[K]) : Boolean = seq.map(s.similar(k,_)).exists(_ > threshhold)
      val ks = self.map(f)
      self.map(a => (a,f(a))).toMap
        .filter{case (a,k) => similarValueExists(k,ks)}
        .groupBy(_._2)
        .mapValues(as => as.map(_._1).toIndexedSeq)
    } //http://stackoverflow.com/questions/2338282/elegant-way-to-invert-a-map-in-scala
      //not sure what I just did here
      //function returns a map of K to Seq[A] generated by taking the A's that map to
      //very similar K's (determined by the threshold value) and grouping them together
  }


  case class Word(value: String) extends AnyVal

  /*
  "hello"
  ngrams where 1-gram, 2-gram, etc
  1-gram { "h","e","l","l","o" } (set of shingles or 'shingle set')
  2-gram { "he","el","ll","lo" }
  ...
  n-gram { ... }

  "hlelo" (typo)
  2-gram { "hl","le","el","lo" }

  jaccard's forumla = (# of matches, total in the set) = 0 to 100

  2,3-gram => 2-gram union 1-gram (best for short strings)
  3,4-gram => 3-gram union 4-gram (best for longer)
  5,6-gram => 5-gram union 6-gram (longest)

  weighted shingling
  type WeightedShingle = (String,Int)

  case class Address(
    street: String,
    streetNumber: String,
    countryCode: String,
    postalCode: String
  )

  shingle set for Address = shingle set of street union streetNumbe union countryCode, ...

  streetNumber because it contains numbers is very identifying but will be one of the shortest
  street is not as identifying since many streets just happen to be the same but it is longest string

  weighted shingle 1 => normal weight, 2+ more significance

  weighted shingle formula similar to jaccard's formula to compute the similarity from weighted shingles

   */
  implicit class SMach_Similar_PimpMyString(val self: String) extends AnyVal {

    def chargrams: Iterator[Char] = self.toCharArray.iterator

    def wordgrams: Iterator[Word] = self.split(" ").filter(_.nonEmpty).map(Word).iterator

    def ngrams(matcher: Regex) : Iterator[String] = {
      matcher.findAllMatchIn(self).map(_.toString).toIterator
    }
  }

  implicit class SMach_Similiar_PimpEverything[A](val self: A) extends AnyVal {

    def similar(rhs: A)(implicit canSimilar: CanSimilar[A]) : Double = {
      canSimilar.similar(self, rhs)
    }

    def shingle[S](implicit shingler: Shingler[A,S]) : List[S] = {
      shingler.shingle(self)
    }
  }

  implicit class SMach_Similar_PimpMyIterable[A](val self: Iterator[A]) extends AnyVal {
    def ksliding(k_range: Range) = k_range.iterator.flatMap(self.sliding(_,1))
  }
}