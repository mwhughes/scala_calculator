package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map{
      case (p, q) => 
        
        (p, Signal(eval(q(), namedExpressions))
          
      )
    }
  }
  
 
  
  
  
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    
    def cyclicReference(expr2: Expr, referenced: Set[String]) : Double ={ 
      
        expr2 match {
          case Literal(v) => v
          case Ref(name) => {
            if (referenced contains name) { return Double.NaN} 
            else {
              return cyclicReference(getReferenceExpr(name, references), referenced+name)
            }
            
          }
          case Plus(a, b) => cyclicReference(a, referenced) + cyclicReference(b, referenced)
          case Minus(a, b) => cyclicReference(a, referenced) - cyclicReference(b, referenced)
          case Times(a, b) => cyclicReference(a, referenced) * cyclicReference(b, referenced)
          case Divide(a, b) => cyclicReference(a, referenced) / cyclicReference(b, referenced)
          
      }
    }
      
    cyclicReference(expr, Set())
     
  
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
