
 /** The algebraic data types for the various language elements.*/


package org.jetho.scms


sealed abstract class Exp

case class SymbolExp(name: String) extends Exp {
  override def toString = name
}

case class ListExp(elems: List[Exp]) extends Exp {
  override def toString = "(" + elems.mkString(" ") + ")"
}

case class DottedListExp(head : List[Exp], tail : Exp) extends Exp {
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
