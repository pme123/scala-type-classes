package geekabyte


object CustomCheck extends App {

  case class Dutch()

  case class German()

  trait EUPassport[T] {
    def validPassport(traveller: T): Boolean
  }

  implicit object DutchPassport extends EUPassport[Dutch] {
    // sure in real life, there would be real validating logic
    override def validPassport(traveler: Dutch): Boolean = true
  }

  implicit object GermanPassport extends EUPassport[German] {
    // sure in real life, there would be real validating logic
    override def validPassport(traveler: German): Boolean = true
  }

  object EUCustomCheck {
    def control[T: EUPassport](traveller: T): Boolean =
    // implicitly[EUPassport[T]] allows us to grab a hold
    // of the implicit that would be in scope due to the use
    // of the context bound
      implicitly[EUPassport[T]].validPassport(traveller)
  }

  // still works because the implicit is automatically resolved
  println("Dutch: " + EUCustomCheck.control(Dutch()))
  println("German: " + EUCustomCheck.control(German()))
}