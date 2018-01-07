package geekabyte

object InterfaceObjectDirectAccess extends App {

  // type class
  trait Reversible[A] {
    def reverse(data: A): A
  }

  // type class instance for Int
  implicit object intReversible extends Reversible[Int] {
    override def reverse(data: Int) = data.toString.reverse.toInt
  }

  // type class instance for String
  implicit object stringReversible extends Reversible[String] {
    override def reverse(data: String) = data.reverse
  }

  // basic object interface
  object reverser {

    def apply[T](implicit reversible: Reversible[T]) = reversible

  }

  // usage of the object interface
  println("String reverser: " + reverser[String].reverse("Ajala"))
  println("Int reverser: " + reverser[Int].reverse(12345))

}