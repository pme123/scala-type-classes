package category

import category.entity._

import scala.language.higherKinds

trait Applicative[Box[_]] extends Functor[Box] {

  def pure[A](a: A): Box[A]

  def ap[A, B](boxF: Box[A => B])(boxA: Box[A]): Box[B]

  override def map[A, B](boxA: Box[A])(f: A => B): Box[B] =
    ap[A, B](pure(f))(boxA)

}

object CatApplicativeExample extends App {

  object ApplicativeInstances {

    implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {

      override def pure[A](a: A): Maybe[A] = Just(a)

      override def ap[A, B](boxF: Maybe[A => B])(boxA: Maybe[A]): Maybe[B] = (boxF, boxA) match {
        case (Just(f), Just(a)) => pure(f(a))
        case _ => Empty
      }
    }

    implicit val zeroOrMoreApplicative: Applicative[ZeroOrMore] = new Applicative[ZeroOrMore] {

      override def pure[A](a: A): ZeroOrMore[A] = OneOrMore(a, Zero)

      override def ap[A, B](boxF: ZeroOrMore[A => B])(boxA: ZeroOrMore[A]): ZeroOrMore[B] = (boxF, boxA) match {
        case (OneOrMore(f, _), OneOrMore(a, tail)) => OneOrMore(f(a), ap(pure(f))(tail))
        case _ => Zero
      }
    }

  }

  import ApplicativeInstances._

  println("maybeApplicative pure: " + maybeApplicative.pure(12))
  println("zeroOrMoreApplicative pure: " + zeroOrMoreApplicative.pure(2))

  println("maybeApplicative ap: " + maybeApplicative.ap(Just((a: Int) => 2 * a))(Just(3)))
  println("zeroOrMoreApplicative ap: " + zeroOrMoreApplicative.ap(OneOrMore((a: Int) => 2 * a, Zero))(OneOrMore(12, OneOrMore(5, Zero))))


}
