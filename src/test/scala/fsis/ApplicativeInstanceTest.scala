package fsis

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary
import ApplicativeInstances._

abstract class ApplicativeInstanceTest[F[_]](name: String)(implicit
  F: Applicative[F],
  eqFInt: Equal[F[Int]],
  eqFStr: Equal[F[String]],
  arbFInt: Arbitrary[F[Int]],
) extends Properties(s"Applicative[$name]") {

  val laws = ApplicativeLaws[F]

  property("applicative identity") = forAll { (xs: F[Int]) =>
    laws.applicativeIdentity(xs).isEqual
  }

  property("applicative homomorphism") = forAll { (xs: Int, f: Int => String) =>
    laws.applicativeHomomorphism(xs, f).isEqual
  }

  property("applicative interchange") = forAll { (xs: Int, f: Int => Int) =>
    laws.applicativeInterchange(xs, F.pure(f)).isEqual
  }

  property("applicative map") = forAll { (a: Int, f: Int => Int) =>
    laws.applicativeMap(F.pure(a), f).isEqual
  }
}

object ListApplicativeTest extends ApplicativeInstanceTest[List]("List")
object OptionApplicativeTest extends ApplicativeInstanceTest[Option]("Option")