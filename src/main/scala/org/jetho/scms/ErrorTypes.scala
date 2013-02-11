
package org.jetho.scms


sealed abstract class Error

case class NumArgs(expected: Int, values: List[Exp]) extends Error {
  override def toString = s"Expected $expected args, found: " + values.mkString(" ")
}

case class TypeMismatch(expected: String, found: Exp) extends Error {
  override def toString = s"Invalid Type: expected $expected, found $found"
}

case class ParseError(msg: String) extends Error {
  override def toString = "Parse Error: " + msg
}

case class BadSpecialForm(msg: String, exp: Exp) extends Error {
  override def toString = s"$msg: $exp"
}

case class NotFunction(msg: String, func: String) extends Error {
  override def toString = s"$msg: $func"
}

case class UnboundVar(msg: String, name: String) extends Error {
  override def toString = s"$msg: $name"
}

case class Default(msg: String) extends Error {
  override def toString = msg
}

