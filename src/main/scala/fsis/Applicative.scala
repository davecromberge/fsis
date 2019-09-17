package fsis

import simulacrum._

// For in-depth description, see the paper "Applicative programming with Effects" by
// Connor and Paterson.
// Simulacrum's typeclass annotation does not compile due to F[A => B] in `flip`,
// therefore no syntax is available in the laws implementation.
trait Applicative[F[_]] extends Functor[F] { self =>

  // takes a value outside of effect system and brings it into effect system
  def pure[A](a: A): F[A]

  // similar to map definition, but it is more general.  Applicatives operate
  // within the container effect F, whereas functors unwrap and rewrap values
  // into F.
  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a, b))))

  // derived functions
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  // mapN instances imply that there is no sequencing, ie dependency 
  // on the result of the first operation before the second
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => f(_, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((_, _, _))

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      override def pure[A](a: A): F[G[A]] =
        self.pure(G.pure(a))
      override def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
        val x: F[G[A] => G[B]] = self.map(ff)(gab => G.flip(gab))
        // we know that we need to use the apply of F because the outer effect
        // type required is that of F
        self.apply(fga)(x)
      }
    }

  // See definition of lift in Functor:
  //  lift[A, B](f: A => B): F[A] => F[B] =
  // The first argument is curried, and then applied
  def flip[A, B](ff: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(ff)
}

object ApplicativeInstances {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Option.apply(a)
    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (None, _) => None
      case (Some(a), None) => None
      case (Some(a), Some(f)) => Some(f(a))
    }
  }
  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }
  implicit val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    override def pure[A](a: A): Stream[A] = Stream.continually(a)
    override def apply[A, B](fa: Stream[A])(ff: Stream[A => B]): Stream[B] =
      (fa zip ff).map { case (a, f) => f(a) }
  }
}

trait ApplicativeLaws[F[_]] {
  import fsis.IsEq._

  implicit def F: Applicative[F]

  // pure lifts identity function A => A into F[A => A]
  def applicativeIdentity[A](fa: F[A]) =
    F.apply(fa)(F.pure((a: A) => a)) =?= fa

  // Function application distributes over apply and pure. We can apply
  // a function to a value outside and then lift, or we can lift the
  // value and apply the lifted function independantly.
  def applicativeHomomorphism[A, B](a: A, f: A => B) =
    F.apply(F.pure(a))(F.pure(f)) =?= F.pure(f(a))

  // Lifting A and applying ff to it gives the same result as lifting a function
  // A => B and applying it to ff.
  // Substituting types in the case where apply(ff) is used:
  // F[(A => B) => B]
  def applicativeInterchange[A, B](a: A, ff: F[A => B]) =
    F.apply(F.pure(a))(ff) =?= F.apply(ff)(F.pure((f: A => B) => f(a)))

  // Map operation must be consistent with apply and pure.
  def applicativeMap[A, B](fa: F[A], f: A => B) =
    F.map(fa)(f) =?= F.apply(fa)(F.pure(f))
}

object ApplicativeLaws {
  def apply[F[_]](implicit F0: Applicative[F]): ApplicativeLaws[F] = new ApplicativeLaws[F] {
    override def F = F0
  }
}