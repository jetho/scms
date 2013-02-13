
 /** The Evaluator.*/


package org.jetho.scms


import scalaz._
import Scalaz._
import scala.util.control.Exception._


object Eval {

  type Result[+A] = ErrorMsg \/ A  
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
    case _ => BadSpecialForm("Unrecognized special form", exp).left
  }

  def applyFunc(id: String)(args: List[Exp]) =
    primitives.get(id) some { _.apply(args) } none { NotFunction("Unrecognized primitive function args", id).left }

  def numericBinOp(op: (Int, Int) => Int)(args: List[Exp]) = 
    if (args.length < 2) NumArgs(2, args).left
    else
      for {
        params <- args.map(unpackNum).sequenceU
      } yield NumExp(params.reduceLeft(op))
    
  def unpackNum(exp: Exp): Result[Int] = exp match {
    case NumExp(n) => n.right
    case StringExp(s) => allCatch.either(s.toInt).fold(_ => TypeMismatch("number", exp).left, _.right)
    case ListExp(List(n)) => unpackNum(n)
    case _ => TypeMismatch("number", exp).left
  }

  def numBoolBinOp = boolBinOp(unpackNum) _
  def boolBoolBinOp = boolBinOp(unpackBool) _
  def strBoolBinOp = boolBinOp(unpackStr) _
  
  def boolBinOp[A](unpacker: Exp => Result[A])(op: (A, A) => Boolean)(args: List[Exp]) = 
    if (args.length != 2) NumArgs(2, args).left 
    else
      for {
        left  <- unpacker(args.head)
        right <- unpacker(args.last)
      } yield BoolExp(op(left, right))

  def unpackStr(exp: Exp) = exp match {
    case StringExp(s) => s.right
    case NumExp(n) => n.toString.right
    case BoolExp(b) => b.toString.right
    case _ => TypeMismatch("string", exp).left
  }

  def unpackBool(exp: Exp) = exp match {
    case BoolExp(b) => b.right
    case _ => TypeMismatch("bool", exp).left
  } 

  def car: Primitive = args => args match {
    case List(ListExp(x :: xs)) => x.right
    case List(DottedListExp(x :: xs, _)) => x.right
    case List(other) => TypeMismatch("pair", other).left
    case _ => NumArgs(1, args).left
  }

  def cdr: Primitive = args => args match {
    case List(ListExp(x::xs)) => ListExp(xs).right
    case List(DottedListExp(_ :: xs, x)) => DottedListExp(xs, x).right
    case List(DottedListExp(_, x)) => x.right
    case List(other) => TypeMismatch("pair", other).left
    case _ => NumArgs(1, args).left
  }

  def cons: Primitive = args => args match {
    case List(x, ListExp(Nil)) => ListExp(List(x)).right
    case List(x, ListExp(xs)) => ListExp(x :: xs).right
    case List(x, DottedListExp(xs1, xs2)) => DottedListExp(x :: xs1, xs2).right
    case List(x1, x2) => DottedListExp(List(x1), x2).right
    case _ => NumArgs(2, args).left
  }     

  def eqv: List[Exp] => Result[BoolExp] = args => args match {
   case List(BoolExp(b1), BoolExp(b2)) => BoolExp(b1 == b2).right
   case List(NumExp(n1), NumExp(n2)) => BoolExp(n1 == n2).right
   case List(StringExp(s1), StringExp(s2)) => BoolExp(s1 == s2).right
   case List(SymbolExp(sym1), SymbolExp(sym2)) => BoolExp(sym1 == sym2).right
   case List(DottedListExp(xs, x), DottedListExp(ys, y)) => BoolExp(xs == ys && x == y).right
   case List(ListExp(xs), ListExp(ys)) =>  
     def eqvPair: Pair[Exp, Exp] => Boolean = { case (e1, e2) => 
       eqv(List(e1, e2)) flatMap unpackBool getOrElse false 
     }
     BoolExp((xs.length == ys.length) && (xs.zip(ys) forall eqvPair)).right
   case List(_, _) => BoolExp(false).right
   case _ => NumArgs(2, args).left
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
      "string>=?" -> strBoolBinOp (_ >= _),
      "car" -> car,
      "cdr" -> cdr,
      "cons" -> cons,
      "eqv?" -> eqv
    )  
}
  
