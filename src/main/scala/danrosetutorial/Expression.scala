package danrosetutorial

sealed trait Expression[A] {

  def value(expr: A): Int
}

object ExpressionEvaluator {
  def evaluate[A: Expression](expr: A): Int =
    implicitly[Expression[A]].value(expr)
}


object Expression {
  implicit val intExpression: Expression[Int] = new Expression[Int] {
    override def value(value: Int): Int = value
  }

  implicit def pairPlusExpression[T1: Expression, T2: Expression]: Expression[(T1, T2)] = new Expression[(T1, T2)] {
    override def value(pair: (T1, T2)): Int =
      implicitly[Expression[T1]].value(pair._1) +
        implicitly[Expression[T2]].value(pair._2)
  }
}