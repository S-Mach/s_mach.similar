package s_mach.similar

object TestObjects {


  implicit val intSim = SimilarOps.simByDistanceThreshold[Int](100, (a,b) => Math.abs(a - b))
  implicit val stringSim = SimilarOps.simByDistanceThreshold[String](
    (a,b) => Math.max(a.length, b.length),
    SimilarOps.levenshteinDistance,
    _.isEmpty)
//  val numbers = List(1,10,15,20,24,34,17,18,9,3,2,1,10,4,5,6)
//  val strings = List("Hello", "world", "World", "HELLO")
}
