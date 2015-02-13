package s_mach.similar

import s_mach.similar.shingle._
import s_mach.string._
import org.scalatest.{FlatSpec, Matchers}

class SMach_Similar_ShinglerTests extends FlatSpec with Matchers {
  "ShinglerOps.chargrams" should "create a shingler that shingles strings with lower case char grams by default" in {
    val shingler = (_:String).charShingles(2 to 3)
    shingler("aBcd").toList should equal(List("ab","bc","cd","abc","bcd"))
    shingler("a").toList should equal(Nil)
    shingler("").toList should equal(Nil)
  }

  "ShinglerOps.chargrams" should "create a shingler that shingles strings with case sensitive char grams when selected" in {
    val shingler = (_:String).charShingles(2 to 3,toLowerCase = false)
    shingler("aBcd").toList should equal(List("aB","Bc","cd","aBc","Bcd"))
    shingler("a").toList should equal(Nil)
    shingler("").toList should equal(Nil)
  }

  "ShinglerOps.wordgrams" should "create a shingler that shingles strings with word grams" in {
    import WordSplitter.Whitespace
    val shingler = (_:String).wordShingles(2 to 3).map(_.mkString("|"))
    shingler("Rar smash house good").toList should equal(List(
      "rar|smash",
      "smash|house",
      "house|good",
      "rar|smash|house",
      "smash|house|good"
    ))
    shingler("rar").toList should equal(Nil)
    shingler("").toList should equal(Nil)
  }

  "ShinglerOps.apply" should "create a shingler that shingles any type with any gram type" in {
    val shingler = (_:IndexedSeq[Int]).ksliding(2 to 3)
    shingler(Vector(1,2,3,4)).toList should equal(List(
      IndexedSeq(1,2),
      IndexedSeq(2,3),
      IndexedSeq(3,4),
      IndexedSeq(1,2,3),
      IndexedSeq(2,3,4)
    ))
    shingler(Vector.empty).toList should equal(Nil)
  }

  "Shingler" should "be composeable" in {
    case class Person(first: String, last: String)
    val charGramsShingler = (_:String).charShingles(2 to 3)
    val personShingler = { p:Person =>
      charGramsShingler(p.first).map(s => s"first|$s") ++
      charGramsShingler(p.last).map(s => s"last|$s")
    }
    personShingler(Person("Rar","Smash")).toList should equal(List(
      "first|ra",
      "first|ar",
      "first|rar",
      "last|sm",
      "last|ma",
      "last|as",
      "last|sh",
      "last|sma",
      "last|mas",
      "last|ash"
    ))
  }
}
