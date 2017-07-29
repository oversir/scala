package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  type ExprMap = Map[String, Signal[Expr]]

  def computeValues(exprs: ExprMap): Map[String, Signal[Double]] =
    exprs.mapValues(expr => Signal(eval(expr())(exprs)))
  
  def eval(expr: Expr)(implicit refs: ExprMap): Double = expr match {
    case Literal(v: Double) => v
    case Ref(name: String)  => eval(lookup(name, refs))(refs - name)
    case Plus(a: Expr, b: Expr)   => eval(a) + eval(b)
    case Minus(a: Expr, b: Expr)  => eval(a) - eval(b)
    case Times(a: Expr, b: Expr)  => eval(a) * eval(b)
    case Divide(a: Expr, b: Expr) => eval(a) / eval(b)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def lookup(name: String, refs: ExprMap) =
    refs.get(name).fold[Expr](Literal(Double.NaN))(_())
}
