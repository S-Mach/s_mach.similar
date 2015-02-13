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

}
