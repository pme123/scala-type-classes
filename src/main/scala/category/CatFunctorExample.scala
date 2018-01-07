package category

import category.entity._

import scala.language.higherKinds

trait Functor[Box[_]] {

  def map[A, B](boxA: Box[A])(f: A => B): Box[B]
}

object CatFunctorExample extends App {

  object FunctorInstances {

    implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
      override def map[A, B](boxA: Maybe[A])(f: A => B): Maybe[B] = boxA match {
        case Just(a) => Just(f(a))
        case Empty => Empty
      }
    }

    implicit val zeroOrMoreFunctor: Functor[ZeroOrMore] = new Functor[ZeroOrMore] {
      override def map[A, B](boxA: ZeroOrMore[A])(f: A => B): ZeroOrMore[B] = boxA match {
        case OneOrMore(a, tail) => OneOrMore(f(a), map(tail)(f))
        case Zero => Zero
      }
    }

  }

  import FunctorInstances._

  println("maybeFunctor map: " + maybeFunctor.map(Just(12))(a => a * 2))
  println("zeroOrMoreFunctor map: " + zeroOrMoreFunctor.map(OneOrMore(12, OneOrMore(5, Zero)))(a => a * 2))


}
