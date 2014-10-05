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

    "toList" should {
      "return a List" in {
        val r: List[String] = toList(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c"))))
        r must containTheSameElementsAs (List[String]("a", "b", "c"))
      }
    }
  }

  "OptionFoldable" >> {
    import OptionFoldable._

    "foldMap" should {
      "return folded value" in {
        foldMap(Some("aa"))(_.size)(intAddition) must be_== (2)
      }
    }

    "foldLeft" should {
      "return folded value" in {
        foldLeft(Some("aa"))(100)((i, str) => i + str.size) must be_== (102)
      }
    }

    "foldRight" should {
      "return folded value" in {
        val r: Int = foldRight(Some("aa"))(100)((str, i) => i + str.size)
        r must be_== (102)
      }
    }
  }
}
