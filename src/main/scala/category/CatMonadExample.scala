package category

import category.entity._

import scala.language.higherKinds

trait Monad[Box[_]] extends Applicative[Box] {

  def flatMap[A, B](boxA: Box[A])(f: A => Box[B]): Box[B]

  def flatten[A](boxBoxA: Box[Box[A]]): Box[A] =
    flatMap(boxBoxA)(identity)

  def ap[A, B](boxF: Box[A => B])(boxA: Box[A]): Box[B] =
    flatMap(boxF)(f => map(boxA)(f))

  override def map[A, B](boxA: Box[A])(f: A => B): Box[B] =
    flatMap(boxA)(a => pure(f(a)))


}

object CatMonadExample extends App {

  object MonadInstances {

    import CatApplicativeExample.ApplicativeInstances._

    implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {

      override def pure[A](a: A): Maybe[A] = maybeApplicative.pure(a)

      override def flatMap[A, B](boxA: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        boxA match {
          case Just(a) => f(a)
          case _ => Empty
        }

    }

    implicit val zeroOrMoreMonad: Monad[ZeroOrMore] = new Monad[ZeroOrMore] {

      override def pure[A](a: A): ZeroOrMore[A] = zeroOrMoreApplicative.pure(a)

      override def flatMap[A, B](boxA: ZeroOrMore[A])(f: A => ZeroOrMore[B]): ZeroOrMore[B] =
        boxA match {
          case OneOrMore(h, tail) => f(h).append(flatMap(tail)(f))
          case _ => Zero
        }

    }

  }

  import MonadInstances._

  println("maybeMonad pure: " + maybeMonad.pure(12))
  println("zeroOrMoreMonad pure: " + zeroOrMoreMonad.pure(2))

  println("maybeMonad ap: " + maybeMonad.flatMap(Just(3))((a: Int) => Just(2 * a)))
  println("zeroOrMoreMonad ap: " + zeroOrMoreMonad.flatMap(OneOrMore(12, OneOrMore(5, Zero)))((a: Int) => OneOrMore(2 * a, Zero)))


}
