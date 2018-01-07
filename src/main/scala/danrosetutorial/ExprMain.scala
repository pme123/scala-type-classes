package danrosetutorial

object ExprMain extends App {
  val foo = (2, (3, 4))

  println("Expression of foo: " + ExpressionEvaluator.evaluate(foo))
  println("Json of foo: " + JsonWriter.write(foo))
}


