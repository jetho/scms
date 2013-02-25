
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
    case ListExp(SymbolExp("define") :: ListExp(SymbolExp(name) :: params) :: body) =>
      makeNormalFunc(env, params, body) >>= env.define(name)
    case ListExp(SymbolExp("define") :: DottedListExp(SymbolExp(name) :: params, varargs) :: body) =>
      makeVarargs(varargs)(env, params, body) >>= env.define(name)
    case ListExp(SymbolExp("lambda") :: ListExp(params) :: body) =>
      makeNormalFunc(env, params, body)
    case ListExp(SymbolExp("lambda") :: DottedListExp(params, varargs) :: body) =>
      makeVarargs(varargs)(env, params, body)
    case ListExp(SymbolExp("lambda") :: (varargs @ SymbolExp(_)) :: body) => 
      makeVarargs(varargs)(env, Nil, body)
    case ListExp(List(SymbolExp("load"), StringExp(filename))) =>
      Reader.loadAndParse(filename) >>= (_.map(eval(env)).sequenceU map { _.last })
    case ListExp(f :: args) => 
      for {
        func <- eval(env)(f)
        params <- args.map(eval(env)).sequenceU
        result <- applyFunc(func)(params)
      } yield result
    case _ => BadSpecialForm("Unrecognized special form", exp).left
  }


  private def makeFunc (varargs: Option[String])(env: Environment, params: List[Exp], body: List[Exp]): Result[Exp] = 
    Func(params map {_.toString}, varargs, body, env).right
  
  private def makeNormalFunc = makeFunc(None) _

  private def makeVarargs(exp: Exp) = makeFunc(exp.toString.some) _

  private[scms] def applyFunc(func: Exp)(args: List[Exp]) = func match {
    case PrimitiveFunc(f) => f(args)
    case IOFunc(f) => f(args)
    case Func(params, varargs, body, closure) =>
      if (params.length != args.length && varargs == None)
        NumArgs(params.length, args).left
      else {
        val argBindings = params zip args
        val varargBindings = varargs.map { (_, ListExp(args drop params.length)) }.toList
        val extendedEnv = closure.extend(argBindings ++ varargBindings)
        (body.map(eval(extendedEnv)).sequenceU) map { _.last }
      }
    case _ => NotFunction("Illegal Function Expression", func).left
  }

}
  
