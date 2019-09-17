package fsis

import simulacrum._

trait Monad[F[_]] extends Applicative[F] { self =>

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f =>  map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  // This results in a stack overflow exception since flatten is defined
  // in terms of flatMap, which in turn is defined in terms of flatten.
  // No matter what definition we try and use, we cannot find a lawful
  // way to compose two monads together.  Instead it is possible to use
  // Kleisli arrows to compose two Kleislis, A => F[B] and B => F[C] to
  // A => F[C] (see fp in scala book)
  def compose[G[_]](implicit G: Monad[G]): Monad[Lambda[X => F[G[X]]]] =
    new Monad[Lambda[X => F[G[X]]]] {
      override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))
      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        val nested = self.map(fga)(ga => G.map(ga)(f))
        flatten(nested)
      }
    }
}

object MonadInstances {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }
  implicit val listMonoid: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa flatMap f
  }
}

trait MonadLaws[F[_]] {
  import fsis.IsEq._

  implicit def F: Monad[F]

  // The result of flatmap over fa with f, and flatMapping over that fb should be the
  // same as distributing over the functions f and g
  // The LHS is sequential application, and in the second case there is nested application.
  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]) =
    F.flatMap(F.flatMap(fa)(f))(g) =?= F.flatMap(F.flatMap(fa)(a => f(a)))(b => g(b))

  def leftIdentity[A, B](a: A, f: A => F[B]) =
    F.flatMap(F.pure(a))(f) =?= f(a)

  def rightIdentity[A](fa: F[A]) =
    F.flatMap(fa)(a => F.pure(a)) =?= fa
}

object MonadLaws {
  def apply[F[_]](implicit F0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    override def F = F0
  }
}