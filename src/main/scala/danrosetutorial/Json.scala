package danrosetutorial

trait Json[A] {
  def json(value: A): JsonValue
}

object JsonWriter {
  def write[A: Json](value:A): String = {
    val json = implicitly[Json[A]].json(value)
    json.print
  }
}

object Json {

  implicit val intJson: Json[Int] = (value: Int) => JsonNumber(value)

  implicit  def pairJson[T1: Json, T2: Json]: Json[(T1, T2)] = (pair: (T1, T2)) => JsonObject(
    Map(
      "fst" -> implicitly[Json[T1]].json(pair._1)
      , "snd" -> implicitly[Json[T2]].json(pair._2)
    )
  )
}
