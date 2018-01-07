package danrosetutorial

sealed trait JsonValue {
  def print: String

}

case class JsonObject(entries: Map[String, JsonValue]) extends JsonValue {
  override def print: String = entries.map(e => s"${e._1}=${e._2.print}").mkString(" {\n", "\n  , ", "\n }\n")
}

case class JsonArray(entries: Seq[JsonValue]) extends JsonValue {
  override def print: String = entries.map(_.print).mkString(" [\n", "\n  , ", "\n ]\n")
}

case class JsonString(value: String) extends JsonValue {
  override def print: String = s"'$value'"
}

case class JsonNumber(value: Double) extends JsonValue {
  override def print: String = f"$value%1.2f"
}

case class JsonBoolean(value: Boolean) extends JsonValue {
  override def print: String = s"$value"
}
