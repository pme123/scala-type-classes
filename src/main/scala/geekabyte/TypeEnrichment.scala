package geekabyte

object TypeEnrichment extends App {

  // type class
  trait Reversible[A] {
    def reverse(data: A): A
  }

  // type class instance for Int
  implicit object intReversible extends Reversible[Int] {
    override def reverse(data: Int): Int = data.toString.reverse.toInt
  }

  // type class instance for String
  implicit object stringReversible extends Reversible[String] {
    override def reverse(data: String): String = data.reverse
  }

  implicit class FlipOps[T](value: T) {
    def flip(implicit reversible: Reversible[T]): T = {
      reversible.reverse(value)
    }
  }

  // usage of the enriched type
  println("String reverser: " + "Ajala".flip)
  println("Int reverser: " + 12345.flip)

}