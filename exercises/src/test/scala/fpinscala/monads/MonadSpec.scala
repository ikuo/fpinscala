package fpinscala
package monads

import Monad._
import org.specs2.mutable._
import org.specs2.specification.Scope
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class MonadSpec extends Specification {
  trait DefaultExecutor extends Scope {
    protected val es = Executors.newFixedThreadPool(3)
  }

  "parMonad" >> {
    "#unit" should {
      "return Par.unit" in new DefaultExecutor {
        parMonad.unit(100)(es).get must === (100)
      }
    }

    "#flatMap" should {
      "return Par.flatMap" in new DefaultExecutor {
        parMonad.flatMap(parMonad.unit(100))(a => parMonad.unit(a + 5))(es).get.
          must(be_==(105))
      }
    }
  }
}
