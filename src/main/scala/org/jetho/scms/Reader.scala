
 /** The S-Expression Parser.*/


package org.jetho.scms


import scala.util.parsing.combinator.RegexParsers
import scalaz._
import Scalaz._
import scalaz.\/._
import scala.io.Source.fromFile


object Reader extends RegexParsers {   

  private def atom = """[a-zA-Z\Q!$%&|*+-/:<=>?@^_~#\E][a-zA-Z\Q!$%&|*+-/:<=>?@^_~#\E0-9]*""".r ^^ { 
    case "#t" => BoolExp(true)
    case "#f" => BoolExp(false)
    case sym => SymbolExp(sym)
  }

  private def num = """-?\d+""".r ^^ { n => NumExp(n.toInt) }

  private def str = "\"" ~> rep("[^\"]".r) <~ "\"" ^^ { s => StringExp(s.mkString) }

  private def quoted = "'" ~> expr ^^ { e => ListExp(List(SymbolExp("quote"), e)) }

  private def dottedList = rep(expr) ~ "." ~ expr ^^ { case head ~ "." ~ tail => DottedListExp(head, tail) }

  private def list = rep(expr) ^^ { ListExp }

  private def expr: Parser[Exp] = atom | str | num | quoted | "(" ~> (dottedList | list) <~ ")"

  private def exprList: Parser[List[Exp]] = rep(expr) <~ "$".r

    
  private def readOrThrow[A](parser: Parser[A])(input: String): ErrorMsg \/ A =
    parse(parser, input) match {
      case Success(res, _) => res.right
      case NoSuccess(msg, _) => ParseError(msg).left 
    }

  def readExpr = readOrThrow[Exp](expr) _

  def readExprList = readOrThrow[List[Exp]](exprList) _

  def load(filename: String): ErrorMsg \/ String = 
    fromTryCatch(fromFile(filename)) bimap (IOErrorMsg, _.mkString)

  def loadAndParse(filename: String) = load(filename) >>= readExprList

  def readAll(filenames: List[Exp]) = filenames match {
    case List(SymbolExp(filename)) => loadAndParse(filename) map ListExp 
  }

}
