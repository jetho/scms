
 /** The Evaluator.*/


package org.jetho.scms


import scalaz._
import Scalaz._


object Eval {

  type Primitive = List[Exp] => Exp

  def eval(exp: Exp): \/[String, Exp] = exp match {
    case n @ NumExp(_) => n.right
    case s @ StringExp(_) => s.right
    case b @ BoolExp(_) => b.right
    case ListExp(List(SymbolExp("quote"), rest)) => rest.right
    case ListExp(SymbolExp(f) :: args) => args.map(eval).sequenceU >>= applyFunc(f)
    case _ => s"Unknown Expression $exp".left
  }

  def applyFunc(id: String)(args: List[Exp]) =
    for {
      f <- !primitives.contains(id) either s"Unknown Function $id" or primitives(id)
    } yield f(args)

  def numericBinOp(op : (Int, Int) => Int)(args: List[Exp]) = {
    val (first :: rest) = args map unpackNum
    NumExp(rest.foldLeft(first)(op))
  }
    
  def unpackNum(e: Exp): Int = e match {
    case NumExp(n) => n
    case StringExp(s) => s.toInt
    case ListExp(List(n)) => unpackNum(n)
    case _ => 0
  }

  val primitives: Map[String, Primitive] = 
    Map( 
      "+" -> numericBinOp (_ + _),
      "-" -> numericBinOp (_ - _),
      "*" -> numericBinOp (_ * _),
      "/" -> numericBinOp (_ / _),
      "mod" -> numericBinOp (_ % _),
      "quotient" -> numericBinOp(_ / _),
      "remainder" -> numericBinOp(_ % _) 
    )  
}
  
