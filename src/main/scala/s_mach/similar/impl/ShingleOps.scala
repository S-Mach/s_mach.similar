package s_mach.similar.impl

import scala.annotation.tailrec
import s_mach.similar.shingle._
import s_mach.string._

object ShingleOps {
  case class KSlidingIterator[A](
    k_range: Range,
    srcSize: Int,
    srcSlice: (Int,Int) => A
  ) extends Iterator[A] {
    var k = k_range.start
    var i = 0

    override def hasNext = {
      @tailrec def loop() : Boolean = {
        if(i < srcSize && i + k <= srcSize) {
          true
        } else {
          if(k < k_range.end) {
            i = 0
            k = k + 1
            loop()
          } else {
            false
          }
        }
      }
      loop()
    }

    override def next() : A = {
      @tailrec def loop() : A = {
        if(i < srcSize && i + k <= srcSize) {
          val retv = srcSlice(i,i+k)
          i = i + 1
          retv
        } else {
          if(k < k_range.end) {
            i = 0
            k = k + 1
            loop()
          } else {
            throw new NoSuchElementException
          }
        }
      }
      loop()
    }
  }
  // Note: This must return Stream (which is Iterable) to be compatible with ShingleSet (which inherits IterableView)
//  def kslidingStream[A](
//    k_range: Range,
//    maxIndex: Int,
//    slice: (Int,Int) => A
//  ) : Stream[A] = {
//    val maxEndIndex = maxIndex + 1
//    def loop(k: Int, i: Int) : Stream[A] = {
//      if(i <= maxIndex && i + k <= maxEndIndex) {
//        slice(i,i+k) #:: loop(k,i+1)
//      } else {
//        if(k < k_range.end) {
//          loop(k+1,0)
//        } else {
//          Stream.empty
//        }
//      }
//    }
//    loop(k_range.start,0)
//  }

  def charShingles(
    s: String,
    k_range: Range,
    toLowerCase: Boolean = true
  ) : Iterator[String] = {
    val grams = s.ksliding(k_range)
    if(toLowerCase) {
      grams.map(_.toLowerCase)
    } else {
      grams
    }
  }

  def wordShingles(
    s: String,
    k_range: Range,
    toLowerCase: Boolean = true
  )(implicit splitter:WordSplitter) : Iterator[IndexedSeq[String]] = {
    val grams = s.toWords.toIndexedSeq.ksliding(k_range)
    if(toLowerCase) {
      grams.map(_.map(_.toLowerCase))
    } else {
      grams
    }
  }

  def ngramShingles[A,Gram](
    a: A,
    k_range: Range,
    toGrams: A => IndexedSeq[Gram]
  ) : Iterator[IndexedSeq[Gram]] =
    toGrams(a).ksliding(k_range)

  def shortStringShingler : Shingler[String,String] =
    (_:String).charShingles(2 to 3).toIndexedSeq

  // TODO: stemming & lemmatization see http://www.thoughtly.co/blog/working-with-text/
  def docStringShingler(implicit splitter:WordSplitter) : Shingler[String,String] =
    (_:String).wordShingles(3 to 4).map(_.mkString("|")).toIndexedSeq

  // TODO: first/last gram shingler
  // TODO: optimal list of words shingler that adjusts k_range depending on number of words
  // TODO: optimal single word shingler that shingles forward/reverse 2-chargram first/last chargrams

  // Select the 2 most min hash codes and pack them into a long
  def minHash2(hashedValues: Traversable[Int]) : Long =
    // TODO
    ???

  // Select the N most min hash codes and pack them into a string of fixed width
  def minHashN(hashedValues: Traversable[Int], n: Int) : String = {
    // sort
    // convert int to bytes
    // encode hex bytes to string
    // expected string length = n * max_hexstring_length_of_int
    // if string length < expected string length pad with leading 0s to preserve string sort order
    // TODO
    ???
  }
}
