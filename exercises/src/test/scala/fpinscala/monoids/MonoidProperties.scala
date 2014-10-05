package fpinscala.monoids

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._
import org.scalacheck.Gen

class MonoidProperty extends Properties("wcMonoid") {
  import Monoid._

  def genWC: Gen[WC] = oneOf(
    for (str <- alphaStr) yield Stub(str),
    for {
      str1 <- alphaStr
      str2 <- alphaStr
      size1 <- choose(0, 50)
    } yield Part(str1, size1, str2)
  )

  property("associative law") = forAll(genWC, genWC, genWC) { (x1: WC, x2: WC, x3: WC) =>
    wcMonoid.op(wcMonoid.op(x1, x2), x3) == wcMonoid.op(x1, wcMonoid.op(x2, x3))
  }

  property("identity law") = forAll(genWC, genWC, genWC) { (x1: WC, x2: WC, x3: WC) =>
    wcMonoid.op(wcMonoid.op(x1, x2), x3) == wcMonoid.op(x1, wcMonoid.op(x2, x3))
  }
}
