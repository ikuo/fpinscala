package fpinscala.monoids

import org.specs2.mutable._
import Monoid._

class FoldableSpec extends Specification {
  "ListFoldable" >> {
    import ListFoldable._

    "foldRight" should {
      "return folded value" in {
        val result: Int = foldRight(List[String]("aa","b","ccc"))(100)((s, i) => s.size + i)
        result must be_== (106)
      }
    }

    "foldLeft" should {
      "return folded value" in {
        val result: Int = foldLeft(List[String]("aa","b","ccc"))(100)((i, s) => s.size + i)
        result must be_== (106)
      }
    }

    "foldMap" should {
      "return map-folded value" in {
        val result: Int = foldMap(List[String]("aa","b","ccc"))(_.size)(intAddition)
        result must be_== (6)
      }
    }
  }

  "IndexedSeqFoldable" >> {
    import IndexedSeqFoldable._

    "foldRight" should {
      "return folded value" in {
        val result: Int = foldRight(Array[String]("aa","b","ccc"))(100)((s, i) => s.size + i)
        result must be_== (106)
      }
    }

    "foldLeft" should {
      "return folded value" in {
        val result: Int = foldLeft(Array[String]("aa","b","ccc"))(100)((i, s) => s.size + i)
        result must be_== (106)
      }
    }

    "foldMap" should {
      "return map-folded value" in {
        val result: Int = foldMap(Array[String]("aa","b","ccc"))(_.size)(intAddition)
        result must be_== (6)
      }
    }
  }

  "StreamFoldable" >> {
    import StreamFoldable._

    "foldRight" should {
      "return folded value" in {
        val result: Int = foldRight(Stream[String]("aa","b","ccc"))(100)((s, i) => s.size + i)
        result must be_== (106)
      }
    }

    "foldLeft" should {
      "return folded value" in {
        val result: Int = foldLeft(Stream[String]("aa","b","ccc"))(100)((i, s) => s.size + i)
        result must be_== (106)
      }
    }
  }

  "TreeFoldable" >> {
    import TreeFoldable._

    "foldMap" should {
      "return folded value" in {
        val result: Int = foldMap(Leaf("aa"))(_.size)(intAddition)
        result must be_== (2)
      }
    }

    "foldLeft" should {
      "return folded value" in {
        val result: Int = foldLeft(Leaf("aa"))(100)((i, s) => s.size + i)
        result must be_== (102)
      }
    }

    "foldRight" should {
      "return folded value" in {
        val result: Int = foldRight(Leaf("aa"))(100)((s, i) => s.size + i)
        result must be_== (102)
      }
    }
  }
}
