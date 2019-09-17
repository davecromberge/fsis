package fsis

import simulacrum._

// A proper type is a value that can be applied to an expression in our language.
// Kinds can be inspected with :k in the scala repl.  A higher-kinded type can be
// turned into a proper type through application to a proper type, in the case of
// type constructors such as F[_].  Applied types are the result of applying a
// proper type to a type constructor.

// A functor a typeclass that abstracts over type constructors that provides a map
// function. The map operation needs to adhere to the law of identity and composition.

// Simulacrum defines convenience syntax and summoner functions via a macro defined 
// for @simulacrum annotation 
@typeclass trait Functor[F[_]] { self =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived operations lift, as, void defined in terms of map, given that
  // the laws are true

  // lift prevents us needing to rewrite existing functions A => B and 
  // reuse in the context of F
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] =
    map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] =
    as(fa, ())

  // Lambda[X => F[G[X]] is synonymous with F[G[?]], which goes
  // beyond the capabilities of the kind projector plugin.  This
  // acts as a type constructor satisfying the typing of Functor as F[_].
  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] = 
    new Functor[Lambda[X => F[G[X]]]] {
      override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fga)(ga => G.map(ga)(f))
    }
}

trait FunctorLaws[F[_]] {
  import fsis.IsEq._
  import Functor.ops._
  
  implicit def F: Functor[F]

  def identity[A](fa: F[A]) =
    fa.map(a => a) =?= fa

  def composition[A, B, C](fa: F[A], f: A => B, g: B => C) =
    fa.map(f).map(g) =?= fa.map(f andThen g)
}

object FunctorLaws {
  def apply[F[_]](implicit F0: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
    override def F = F0
  }
}

object FunctorInstances {
  // instances of the Functor typeclass 
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  // X => ? is a type constructor that when applied to a proper type A becomes
  // an applied type X => A
  // We cannot express a functor over Function1 as Functor[Function1], as 
  // Function1 is a binary constructor i.e. a proper type and not a type cons.
  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }
}
