package fpinscala.monoids

import org.scalacheck._
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.map(aa1 => a2.getOrElse(aa1))
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = {
      a => f1.andThen(f2)(a)
    }
    val zero: A => A = { a => a }
  }

  import Prop.forAll
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen, gen, gen) { (a1, a2, a3) =>
      m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
    } &&
    forAll(gen) { (a) =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a
    }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.size match {
      case 0 => m.zero
      case 1 => f(as(0))
      case i => {
        val pivot: Int = i / 2
        m.op(
          foldMapV(as.slice(0, pivot), m)(f),
          foldMapV(as.slice(pivot, as.size), m)(f)
        )
      }
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC {
    def numWords: Int
    protected def isValidWord(word: String): Boolean =
      word.size > 0 && word.forall(_ != ' ')
  }
  case class Stub(chars: String) extends WC {
    def numWords = if (isValidWord(chars)) 1 else 0
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def numWords = {
      var results = words
      if (isValidWord(lStub)) { results += 1 }
      if (isValidWord(rStub)) { results += 1 }
      results
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(x1: WC, x2: WC): WC = (x1, x2) match {
      case (Stub(str1), Stub(str2)) => Part("", numCenterWords(str1, str2), "")

      case (Stub(str1), Part(lStub2, words2, rStub2)) =>
        Part("", words2 + numCenterWords(str1, lStub2), rStub2)

      case (Part(lStub1, words1, rStub1), Stub(str2)) =>
        Part(lStub1, words1 + numCenterWords(rStub1, str2), "")

      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) => {
        Part(lStub1, words1 + words2 + numCenterWords(rStub1, lStub2), rStub2)
      }
    }

    override val zero = Stub("")

    private def numCenterWords(str1: String, str2: String): Int =
      if (str1.endsWith(" ") || str2.startsWith(" ")) 2
      else 1
  }

  def count(s: String): Int = {
    val wc = foldMapV(s.grouped(7).toIndexedSeq, wcMonoid) { str: String =>
      val words = str.split(" ")
      words.size match {
        case 0 => wcMonoid.zero
        case 1 => Stub(words(0))
        case i => Part(words(0), i - 2, words.last)
      }
    }
    wc.numWords
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(Nil: List[A])((l, a) => a :: l)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as.map(f))(mb.zero)(mb.op(_, _))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as.map(f))(mb.zero)(mb.op(_, _))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) =>
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

