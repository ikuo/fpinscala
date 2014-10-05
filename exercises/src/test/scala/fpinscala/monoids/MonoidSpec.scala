package fpinscala.monoids

import org.specs2.mutable._

class MonoidSpec extends Specification {
  import Monoid._
  "foldMap" should {
    "apply the monoid on the list with mapping" in {
      Monoid.foldMap(List[String]("3","2","5"), intAddition)(s => s.toString.toInt).
        must(be_== (10))
    }

    "apply the monoid on the indexed sequence with mapping" in {
      Monoid.foldMapV(Array[String]("1","5","3"), intAddition)(s => s.toString.toInt).
        must(be_== (9))
      Monoid.foldMapV(Array[String]("1","5","3", "8", "12"), intAddition)(s => s.toString.toInt).
        must(be_== (29))
    }
  }
}
