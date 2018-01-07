package category


trait Monoid[A] {

  def identity: A

  def compose(x: A, y: A): A
}

object CatMonoidExample extends App{

  object MonoidInstances {

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def identity: Int = 0

      override def compose(x: Int, y: Int): Int = x + y
    }

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {

      override def identity: String = ""

      override def compose(x: String, y: String): String = x + y

    }

  }


  println("IntMonoid identity: " + MonoidInstances.intMonoid.identity)
  println("IntMonoid compose: " + MonoidInstances.intMonoid.compose(3, 7))

  println("StringMonoid identity: " + MonoidInstances.stringMonoid.identity)
  println("StringMonoid compose: " + MonoidInstances.stringMonoid.compose("hello", "World"))

}
