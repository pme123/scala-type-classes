package selector

import cats.Foldable
import cats.instances.list._
import cats.instances.option._

import scala.language.higherKinds

object GenericSelector extends App {

  trait Named[T] {
    def name(t: T): String
  }

  trait Identified[T] {
    def uid(t: T): String
  }

  abstract class GenericSelector[F[_] : category.Applicative : Foldable, T: Named : Identified] {

    case class Props(
                      selected: F[T]
                      , onChange: F[T] => Unit
                      , all: List[T]
                    ) {

      def isSelected(elem: T): Boolean =
        implicitly[Foldable[F]]
          .exists(selected)(sel => implicitly[Identified[T]].uid(sel) == implicitly[Identified[T]].uid(elem))

    }

    def id: String

    def props: Props

    def renderElement(elem: T): String = {

      s"${implicitly[Named[T]].name(elem)} ${if (props.isSelected(elem)) "selected" else ""}"
    }

    def render(): String = s"$id\n ${props.all.map("\t" + renderElement(_)).mkString("\n")}"

  }


  sealed trait Currency {
    def name: String

    def uid: String
  }

  case object EURO extends Currency {
    override val name = "Euro"

    override val uid = "euro"
  }

  case object DOLLAR extends Currency {
    override val name = "Dollar"

    override val uid = "dollar"
  }

  case object CHF extends Currency {
    override val name = "Swiss Francs"

    override val uid = "chf"
  }

  implicit object Currency
    extends Named[Currency]
      with Identified[Currency] {

    override def name(c: Currency) = c.name

    override def uid(c: Currency) = c.uid


    val all = List(CHF, DOLLAR, EURO)

  }


  // Selector with exact one selected element
  val strictCurrencySelector = new GenericSelector[cats.Id, Currency]() {

    val id = "strictCurrencySelector"

    val props = Props(EURO
      , (c: Currency) => println(s"strict currency changed to $c")
      , Currency.all)

  }

  // Selector with one optional selected element
  val optionalCurrencySelector = new GenericSelector[Option, Currency]() {

    val id = "optionalCurrencySelector"

    val props = Props(Some(CHF)
      , (c: Option[Currency]) => println(s"optional currency changed to $c")
      , Currency.all)

  }

  // Selector with one optional selected element
  val multiCurrencySelector = new GenericSelector[List, Currency]() {

    val id = "multiCurrencySelector"

    val props = Props(List(CHF, EURO)
      , (c: List[Currency]) => println(s"list currency changed to $c")
      , Currency.all)

  }


  println(strictCurrencySelector.render())

  println(optionalCurrencySelector.render())

  println(multiCurrencySelector.render())
}