
package org.jetho.scms

import scalaz._
import Scalaz._
import scalaz.\/._
import Function.const
import scala.io.Source._
import java.io._
import language.reflectiveCalls


object Primitives {

  type Result[+A] = ErrorMsg \/ A  
  type Primitive = List[Exp] => Result[Exp]
  
  
  def primitives = primitivesList map makeFunc(PrimitiveFunc)  

  def ioPrimitives = ioPrimitivesList map makeFunc(IOFunc)


  private def makeFunc(constr: Primitive => Exp)(f: (String, Primitive)) = (f._1, constr(f._2))

  private def numericBinOp(op: (Int, Int) => Int)(args: List[Exp]) = 
    if (args.length < 2) NumArgs(2, args).left
    else
      for {
        params <- args.map(unpackNum).sequenceU
      } yield NumExp(params.reduceLeft(op))
    
  private def unpackNum(exp: Exp): Result[Int] = exp match {
    case NumExp(n) => n.right
    case StringExp(s) => fromTryCatch(s.toInt) leftMap const(TypeMismatch("number", exp))
    case ListExp(List(n)) => unpackNum(n)
    case _ => TypeMismatch("number", exp).left
  }

  private def numBoolBinOp = boolBinOp(unpackNum) _
  private def boolBoolBinOp = boolBinOp(unpackBool) _
  private def strBoolBinOp = boolBinOp(unpackStr) _
  
  private def boolBinOp[A](unpacker: Exp => Result[A])(op: (A, A) => Boolean)(args: List[Exp]) = 
    if (args.length != 2) NumArgs(2, args).left 
    else
      for {
        left  <- unpacker(args.head)
        right <- unpacker(args.last)
      } yield BoolExp(op(left, right))

  private def unpackStr(exp: Exp) = exp match {
    case StringExp(s) => s.right
    case NumExp(n) => n.toString.right
    case BoolExp(b) => b.toString.right
    case _ => TypeMismatch("string", exp).left
  }

  private def unpackBool(exp: Exp) = exp match {
    case BoolExp(b) => b.right
    case _ => TypeMismatch("bool", exp).left
  } 

  private def car: Primitive = args => args match {
    case List(ListExp(x :: xs)) => x.right
    case List(DottedListExp(x :: xs, _)) => x.right
    case List(other) => TypeMismatch("pair", other).left
    case _ => NumArgs(1, args).left
  }

  private def cdr: Primitive = args => args match {
    case List(ListExp(x::xs)) => ListExp(xs).right
    case List(DottedListExp(_ :: xs, x)) => DottedListExp(xs, x).right
    case List(DottedListExp(_, x)) => x.right
    case List(other) => TypeMismatch("pair", other).left
    case _ => NumArgs(1, args).left
  }

  private def cons: Primitive = args => args match {
    case List(x, ListExp(Nil)) => ListExp(List(x)).right
    case List(x, ListExp(xs)) => ListExp(x :: xs).right
    case List(x, DottedListExp(xs1, xs2)) => DottedListExp(x :: xs1, xs2).right
    case List(x1, x2) => DottedListExp(List(x1), x2).right
    case _ => NumArgs(2, args).left
  }     

  private def eqv: List[Exp] => Result[BoolExp] = args => args match {
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

  private def applyProc: Primitive = args => args match {
    case List(f, ListExp(params)) => Eval.applyFunc(f)(params)
    case f :: params => Eval.applyFunc(f)(params)
  }

  private def makePort(constr: String => Exp): Primitive = args => args match {
    case List(StringExp(fname)) => fromTryCatch(constr(fname)) leftMap IOErrorMsg
    case List(s) => TypeMismatch("string", s).left
  }
  
  private def closePort: Primitive = args => {
    def closeMe(p:{ def close(): Unit; }) = 
      fromTryCatch(p.close()) bimap (IOErrorMsg, const(BoolExp(true)))
    args match {
      case List(InPort(p)) => closeMe(p)
      case List(OutPort(p)) => closeMe(p)
      case _ => BoolExp(false).right
    }
  }  

  private def readContents: Primitive = args => args match {
    case List(StringExp(fname)) => fromTryCatch(fromFile(fname).getLines.mkString) bimap (IOErrorMsg, StringExp)
    case List(s) => TypeMismatch("string", s).left
  }

  private def readProc: Primitive = args => args match {
    case Nil => readProc(List(InPort(createBufferedSource(System.in).bufferedReader)))
    case List(InPort(p)) => Reader.readExpr(p.readLine)
  }
  
  private def writeProc: Primitive = args => args match {
    case List(obj) => writeProc(List(obj, OutPort(new BufferedWriter(new PrintWriter(System.out)))))
    case List(obj, OutPort(p)) => fromTryCatch(p.write(obj.toString)) bimap (IOErrorMsg, const(BoolExp(true)))
  }


  private def fileReader: String => InPort = fname => InPort(new BufferedReader(new FileReader(fname)))

  private def fileWriter: String => OutPort = fname => OutPort(new BufferedWriter(new FileWriter(fname)))

 
  private val primitivesList: List[(String, Primitive)] = 
    List( 
      ("+", numericBinOp (_ + _)),
      ("-", numericBinOp (_ - _)),
      ("*", numericBinOp (_ * _)),
      ("/", numericBinOp (_ / _)),
      ("mod", numericBinOp (_ % _)),
      ("quotient", numericBinOp(_ / _)),
      ("remainder", numericBinOp(_ % _)),
      ("=", numBoolBinOp (_ == _)),
      ("<", numBoolBinOp (_ < _)),
      (">", numBoolBinOp (_ > _)),
      ("/=", numBoolBinOp (_ != _)),
      (">=", numBoolBinOp (_ >= _)),
      ("<=", numBoolBinOp (_ <= _)),
      ("&&", boolBoolBinOp (_ && _)),
      ("||", boolBoolBinOp (_ || _)),
      ("string=?", strBoolBinOp (_ == _)),
      ("string<?", strBoolBinOp (_ < _)),
      ("string>?", strBoolBinOp (_ > _)),
      ("string<=?", strBoolBinOp (_ <= _)),
      ("string>=?", strBoolBinOp (_ >= _)),
      ("car", car),
      ("cdr", cdr),
      ("cons", cons),
      ("eqv?", eqv)
    )

  private val ioPrimitivesList: List[(String, Primitive)] = 
    List(
      ("apply", applyProc),
      ("open-input-file", makePort(fileReader)),
      ("open-output-file", makePort(fileWriter)),
      ("close-input-port", closePort),
      ("close-output-port", closePort),
      ("read", readProc),
      ("write", writeProc),
      ("read-contents", readContents),
      ("read-all", Reader.readAll)
    )
      
}
