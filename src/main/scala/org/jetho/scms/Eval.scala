
 /** The Evaluator.*/


package org.jetho.scms


import scalaz._
import Scalaz._


object Eval {

  type Result[A] = String \/ A  
  type Primitive[A] = List[Exp] => Result[A]


  def eval(exp: Exp): Result[Exp] = exp match {
    case n @ NumExp(_) => n.right
    case s @ StringExp(_) => s.right
    case b @ BoolExp(_) => b.right
    case ListExp(List(SymbolExp("quote"), rest)) => rest.right
    case ListExp(SymbolExp(f) :: args) => args.map(eval).sequenceU >>= applyFunc(f)
    case _ => s"Unknown Expression: $exp".left
  }

  def applyFunc(id: String)(args: List[Exp]) =
    toRight(primitives.get(id))(s"Unknown Function: $id") >>= (_ apply args)

  def numericBinOp(op : (Int, Int) => Int)(args: List[Exp]) = 
    for {
      params <- args.map(unpackNum).sequenceU
    } yield NumExp(params.reduceLeft(op))
    
  def unpackNum(e: Exp): Result[Int] = e match {
    case NumExp(n) => n.right
    case StringExp(s) => try { s.toInt.right } catch { case _: Throwable => s"Illegal Number: $s".left }
    case ListExp(List(n)) => unpackNum(n)
    case _ => s"Illegal Number: $e".left
  }

  val primitives: Map[String, Primitive[Exp]] = 
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
  
