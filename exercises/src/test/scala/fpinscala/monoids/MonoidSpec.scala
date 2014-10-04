package fpinscala.monoids

import org.specs2.mutable._

class MonoidSpec extends Specification {
  import Monoid._
  "foldMap" should {
    "apply the monoid on the list with mapping" in {
      foldMap(List[String]("3","2","5"), intAddition)(s => s.toString.toInt) must be_== (10)
    }
  }
}
