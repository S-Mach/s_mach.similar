package s_mach.similar

import org.scalatest.{Matchers, FlatSpec}

class SMach_Similar_JaccardIndexTest extends FlatSpec with Matchers {
  "JaccardIndex" should "compute set similarity through implicit Similar[Traversable[A]]" in {
    import JaccardIndex._
    Set(1,2,3) ==~ Set(1,2,3) should equal(1.0)
    List(0.0,0.1,0.2) ==~ List(1.0,2.0,3.0) should equal(0.0)
    Stream("a","b","c") ==~ Stream("a","b","c","d") should equal(3.0/4)
    Seq("a","b","c") ==~ Seq("a","a","b","c") should equal(3.0/4)
    Vector(1,2,3,5) ==~ Vector(1,1,1,1,2,3,4) should equal(3.0/8.0)
  }

  "JaccardIndex" should "compute set similarity commutatively through implicit Similar[Traversable[A]]" in {
    import JaccardIndex._
    List(0.0,0.1,0.2) ==~ List(1,2,3) should equal(List(1.0,2.0,3.0) ==~ List(0.0,0.1,0.2))
    Stream("a","b","c") ==~ Stream("a","b","c","d") should equal(Stream("a","b","c","d") ==~ Stream("a","b","c"))
    Seq("a","b","c") ==~ Seq("a","a","b","c") should equal(Seq("a","a","b","c") ==~ Seq("a","b","c"))
    Vector(1,2,3,5) ==~ Vector(1,1,1,1,2,3,4) should equal(Vector(1,1,1,1,2,3,4) ==~ Vector(1,2,3,5))
  }

  "JaccardIndex.ShortString" should "compute string similarity using 2,3-chargrams" in {
    import JaccardIndex.ShortString._
    stringShingler("aBcd").toSeq should equal(Seq("ab","bc","cd","abc","bcd"))
    "chocolate" ==~ "chocolate" should equal(1.0)
    stringShingler("hello").toSeq should equal(Seq("he", "el", "ll", "lo", "hel", "ell", "llo"))
    "hello" ==~ "hello" should equal(1.0)
    stringShingler("hell").toSeq should equal(Seq("he", "el", "ll", "hel", "ell"))
    "hello" ==~ "hell" should equal(5.0/7.0)
    stringShingler("hlelo").toSeq should equal(Seq("hl", "le", "el", "lo", "hle", "lel", "elo"))
    "hello" ==~ "hlelo" should equal(2.0/12.0)
  }

  "JaccardIndex.DocString" should "compute string similarity using 3,4-wordgrams" in {
    import JaccardIndex.DocString._
    import TestObjects._
    stringShingler("word1 word2 word3 word4").toSeq should equal(Seq("word1|word2|word3","word2|word3|word4","word1|word2|word3|word4"))
    gettys1 ==~ gettys1 should equal(1.0)
    gettys1 ==~ gettys2 should (be >= 0.42 and be <= 0.43)
  }
}
