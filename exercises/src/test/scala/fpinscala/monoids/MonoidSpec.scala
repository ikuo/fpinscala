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

  "count" should {
    "count words" in {
      count("abcd") must be_== (1)
      count("abcd ef") must be_== (2)
      count("abcd efgh") must be_== (2)
      count("abcd efgh ijklm") must be_== (3)
      count("abcd efgh ijklm asdf") must be_== (4)
      count("abcd efgh ijklm asdf  ") must be_== (4)
      count("  abcd efgh ijklm asdf  ") must be_== (4)
    }
  }

  "Stub#numWords" should {
    "be 1 when valid word" in {
      Stub("abc").numWords must be_== (1)
    }
    "be 0 when invalid word" in {
      Stub(" ").numWords must be_== (0)
    }
  }

  "Part#numWords" should {
    "be sum up left and right stubs with mid words" in {
      Part("", 3, "").numWords must be_== (3)
      Part("abc", 3, "").numWords must be_== (4)
      Part("", 3, "def").numWords must be_== (4)
      Part("abc", 3, "def").numWords must be_== (5)
    }
  }
}
