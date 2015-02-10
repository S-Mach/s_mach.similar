package s_mach.similar

import s_mach.string._
import s_mach.similar.relative._
import org.scalatest.{FlatSpec, Matchers}

class SMach_Similar_ShinglerTests extends FlatSpec with Matchers {
  "Shingler.forChargrams" should "create a shingler that shingles strings with lower case char grams by default" in {
    val s = Shingler.forChargrams(2 to 3)
    s.shingle("aBcd").toList should equal(List("ab","bc","cd","abc","bcd"))
  }

  "Shingler.forChargrams" should "create a shingler that shingles strings with case sensitive char grams when selected" in {
    val s = Shingler.forChargrams(2 to 3,toLowerCase = false)
    s.shingle("aBcd").toList should equal(List("aB","Bc","cd","aBc","Bcd"))
  }

  "Shingler.forWordgrams" should "create a shingler that shingles strings with word grams" in {
    import WordSplitter.Whitespace
    val s = Shingler.forWordgrams(2 to 3,_.toWords.map(_.toLowerCase).toIndexedSeq)
    s.shingle("Rar smash house good").toList should equal(List("rar|smash","smash|house","house|good","rar|smash|house","smash|house|good"))
  }

  "Shingler.apply" should "create a shingler that shingles a type with grams" in {
    val s = Shingler[IndexedSeq[Int],Int](2 to 3, v => v)
    s.shingle(Vector(1,2,3,4)).toList should equal(List(
      IndexedSeq(1,2),
      IndexedSeq(2,3),
      IndexedSeq(3,4),
      IndexedSeq(1,2,3),
      IndexedSeq(2,3,4)
    ))
  }

  "Shingler" should "be composeable" in {
    case class Person(first: String, last: String)
    val charGramsShingler = Shingler.forChargrams(2 to 3)
    val personShingler = new Shingler[Person,String] {
      override def shingle(p: Person) = {
        charGramsShingler.shingle(p.first).map(s => s"first|$s") union
        charGramsShingler.shingle(p.last).map(s => s"last|$s")
      }
    }
    personShingler.shingle(Person("Rar","Smash")).toList should equal(List(
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
