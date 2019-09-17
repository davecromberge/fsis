package fsis

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary
import MonadInstances._

abstract class MonadInstanceTest[F[_]](name: String)(implicit
  F: Monad[F],
  eqFInt: Equal[F[Int]],
  eqFStr: Equal[F[String]],
  eqFLong: Equal[F[Long]],
  arbFInt: Arbitrary[F[Int]],
  arbFString: Arbitrary[F[String]],
  arbFLong: Arbitrary[F[Long]]
) extends Properties(s"Monad[$name]") {

  val laws = MonadLaws[F]

  property("flatMap associativity") = forAll { (xs: F[Int], f: Int => F[String], g: String => F[Long]) =>
    laws.flatMapAssociativity(xs, f, g).isEqual
  }

  property("monad left identity") = forAll { (xs: Int, f: Int => F[String]) =>
    laws.leftIdentity(xs, f).isEqual
  } 

  property("monad right identity") = forAll { (xs: F[Int]) =>
    laws.rightIdentity(xs).isEqual
  } 
}

object ListMonadTest extends MonadInstanceTest[List]("List")
object OptionMonadTest extends MonadInstanceTest[Option]("Option")