
 /** The algebraic data types which represent the various language elements.*/


package org.jetho.scms


sealed abstract class Exp

case class SymbolExp(name: String) extends Exp {
  override def toString = name
}

case class ListExp(elems: List[Exp]) extends Exp {
  override def toString = "(" + elems.map(_.toString).mkString(" ") + ")"
}

case class NumExp(i: Int) extends Exp {
  override def toString = i.toString
}

case class StringExp(s: String) extends Exp {
  override def toString = s
}

case class BoolExp(b: Boolean) extends Exp {
  override def toString = if(b) "#t" else "#f"
}
