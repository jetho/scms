
 /** The Evaluator.*/


package org.jetho.scms


import scalaz._
import Scalaz._


object Eval {

  type Result[+A] = ErrorMsg \/ A  


  def eval(env: Environment)(exp: Exp): Result[Exp] = exp match {
    case n @ NumExp(_) => n.right
    case s @ StringExp(_) => s.right
    case b @ BoolExp(_) => b.right
    case SymbolExp(id) => env.lookup(id)
    case ListExp(List(SymbolExp("quote"), rest)) => rest.right
    case ListExp(List(SymbolExp("set!"), SymbolExp(id), form)) => eval(env)(form) >>= env.update(id)
    case ListExp(List(SymbolExp("define"), SymbolExp(id), form)) => eval(env)(form) >>= env.define(id)    
    case ListExp(List(SymbolExp("if"), cond, conseq, alt)) => 
      for {
        cResult <- eval(env)(cond)
        res <- cResult match {
          case BoolExp(true) => eval(env)(conseq)
          case _ => eval(env)(alt)
        }
      } yield res   
    case ListExp(SymbolExp(f) :: args) => args.map(eval(env)).sequenceU >>= applyFunc(f)
    case _ => BadSpecialForm("Unrecognized special form", exp).left
  }

  def applyFunc(id: String)(args: List[Exp]) =
    Primitives.primitives.get(id) some { _.apply(args) } none { NotFunction("Unrecognized primitive function args", id).left }
    
}
  
