package s_mach.similar

import s_mach.similar.impl.SimilarOps
import s_mach.similar.impl.StringDistances.levenshteinDistance

object Levenshtein {
  implicit val stringSimilar = SimilarOps.simString(levenshteinDistance)
}
