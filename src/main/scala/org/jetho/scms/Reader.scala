
 /** The S-Expression Parser.*/


package org.jetho.scms


import scala.util.parsing.combinator.RegexParsers
import scalaz._
import Scalaz._


object Reader extends RegexParsers {

  private def atom = """[a-zA-Z!#$%&|*+-/:<=>?@^_~][a-zA-Z0-9!#$%&|*+-/:<=>?@^_~]*""".r ^^ { 
    case "#t" => BoolExp(true)
    case "#f" => BoolExp(false)
    case sym => SymbolExp(sym)
  }

  private def num = """-?\d+""".r ^^ { n => NumExp(n.toInt) }

  private def str = "\"" ~> rep("[^\"]".r) <~ "\"" ^^ { s => StringExp(s.mkString) }

  private def quoted = "'" ~> expr ^^ { e => ListExp(List(SymbolExp("quote"), e)) }

  private def list = rep(expr) ^^ { ListExp }

  private def dottedList = rep(expr) ~ "." ~ expr ^^ { case head ~ "." ~ tail => new DottedListExp(head, tail) }

  private def expr : Parser[Exp] = atom | str | num | quoted | "(" ~> (list | dottedList) <~ ")"

    
  def apply(input: String): String \/ Exp =
    parse(expr, input) match {
      case Success(res, _) => res.right
      case NoSuccess(msg, _) => msg.left 
    }
}
