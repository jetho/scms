
 /** The algebraic data types for the various language elements.*/


package org.jetho.scms
import scalaz._
import Scalaz._


sealed abstract class Exp

case class SymbolExp(name: String) extends Exp {
  override def toString = name
}

case class ListExp(elems: List[Exp]) extends Exp {
  override def toString = "(" + elems.mkString(" ") + ")"
}

case class DottedListExp(head : List[Exp], tail :Exp) extends Exp {
  override def toString = "(" + head.mkString(" ") + " . " + tail + ")"
}

case class NumExp(i: Int) extends Exp {
  override def toString = i.toString
}

case class StringExp(s: String) extends Exp {
  override def toString = "\"" + s + "\""
}

case class BoolExp(b: Boolean) extends Exp {
  override def toString = if(b) "#t" else "#f"
}

case class PrimitiveFunc(f: List[Exp] => ErrorMsg \/ Exp) extends Exp {
  override def toString = "<primitive>"
}

case class Func(params: List[String], vararg: Option[String], body: List[Exp], closure: Environment) extends Exp {
  override def toString = "(lambda (" + params.mkString(" ") + vararg.map{" . " + _}.getOrElse("") + ") ...)"
}



