
 /** The Evaluator.*/


package org.jetho.scms


import scalaz._
import Scalaz._


object Eval {

  type Result[A] = String \/ A  
  type Primitive = List[Exp] => Result[Exp]


  def eval(exp: Exp): Result[Exp] = exp match {
    case n @ NumExp(_) => n.right
    case s @ StringExp(_) => s.right
    case b @ BoolExp(_) => b.right
    case ListExp(List(SymbolExp("quote"), rest)) => rest.right
    case ListExp(List(SymbolExp("if"), cond, conseq, alt)) => 
      for {
        cResult <- eval(cond)
        res <- cResult match {
          case BoolExp(true) => eval(conseq)
          case _ => eval(alt)
        }
      } yield res   
    case ListExp(SymbolExp(f) :: args) => args.map(eval).sequenceU >>= applyFunc(f)
    case _ => s"Unknown Expression: $exp".left
  }

  def applyFunc(id: String)(args: List[Exp]) =
    toRight(primitives.get(id))(s"Unknown Function: $id") >>= (_ apply args)

  def numericBinOp(op: (Int, Int) => Int)(args: List[Exp]) = 
    for {
      params <- args.map(unpackNum).sequenceU
    } yield NumExp(params.reduceLeft(op))
    
  def unpackNum(e: Exp): Result[Int] = e match {
    case NumExp(n) => n.right
    case StringExp(s) => try { s.toInt.right } catch { case _: Throwable => s"Illegal Number: $s".left }
    case ListExp(List(n)) => unpackNum(n)
    case _ => s"Illegal Number: $e".left
  }

  def numBoolBinOp = boolBinOp(unpackNum) _
  def boolBoolBinOp = boolBinOp(unpackBool) _
  def strBoolBinOp = boolBinOp(unpackStr) _
  
  def boolBinOp[A](unpacker: Exp => Result[A])(op: (A, A) => Boolean)(args: List[Exp]) = 
    if (args.length != 2) ("Expected 2 args, found values: " + args.mkString(" ")).left 
    else
      for {
        left  <- unpacker(args.head)
        right <- unpacker(args.last)
      } yield BoolExp(op(left, right))

  def unpackStr(v: Exp) = v match {
    case StringExp(s) => s.right
    case NumExp(n) => n.toString.right
    case BoolExp(b) => b.toString.right
    case otherType => ("Invalid type: expected String, found " + otherType).left
  }

  def unpackBool(v: Exp) = v match {
    case BoolExp(b) => b.right
    case otherType => ("Invalid type: expected Bool, found " + otherType).left
  }  


  val primitives: Map[String, Primitive] = 
    Map( 
      "+" -> numericBinOp (_ + _),
      "-" -> numericBinOp (_ - _),
      "*" -> numericBinOp (_ * _),
      "/" -> numericBinOp (_ / _),
      "mod" -> numericBinOp (_ % _),
      "quotient" -> numericBinOp(_ / _),
      "remainder" -> numericBinOp(_ % _),
      "=" -> numBoolBinOp (_ == _),
      "<" -> numBoolBinOp (_ < _),
      ">" -> numBoolBinOp (_ > _),
      "/=" -> numBoolBinOp (_ != _),
      ">=" -> numBoolBinOp (_ >= _),
      "<=" -> numBoolBinOp (_ <= _),
      "&&" -> boolBoolBinOp (_ && _),
      "||" -> boolBoolBinOp (_ || _),
      "string=?" -> strBoolBinOp (_ == _),
      "string<?" -> strBoolBinOp (_ < _),
      "string>?" -> strBoolBinOp (_ > _),
      "string<=?" -> strBoolBinOp (_ <= _),
      "string>=?" -> strBoolBinOp (_ >= _)
    )  
}
  
